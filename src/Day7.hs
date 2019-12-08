{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleInstances,
  TypeApplications, ScopedTypeVariables, NamedFieldPuns,
  RecordWildCards, DataKinds, GADTs, KindSignatures, TypeOperators #-}
module Day7 where

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import Data.Proxy
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Foldl (Fold(..))
import qualified Control.Foldl as Foldl
import Streaming (Of, Stream)
import qualified Streaming
import qualified Streaming.Prelude
import Data.Functor.Identity
import GHC.TypeLits

data Halt = Run | Halt deriving (Show, Read, Eq)
data Mode = Positional | Immediate deriving (Show, Read, Eq)

type Code = IntMap Int
type Addr = Int
type Input = Int
type Output = Int
type Interpreter = ExceptT String (State IntcodeVM)
type Op = [Mode] -> Interpreter (Maybe Int)

data IntCodeOpWithInput (inputs :: Nat) where
  NoInputs :: IntCodeOp n -> IntCodeOpWithInput (0 :: Nat)
  WithInput :: Int -> IntCodeOpWithInput n -> IntCodeOpWithInput (n + 1)

data IntCodeOp (parameters :: Nat) where
  NoParameters :: Op -> IntCodeOp (0 :: Nat)
  WithParameters :: Int -> IntCodeOp n -> IntCodeOp (n + 1)

data IntcodeVM = IntcodeVM
  { haltState :: Halt
  , code :: Code
  , instrPointer :: Int
  }

dumpVM :: IntcodeVM -> String
dumpVM vm@IntcodeVM{..} =
  let haltS = "Halt state: " ++ show haltState
      addrS = "At address: " ++ show instrPointer
      codeS = "With code: " ++ show code
   in unlines [haltS, addrS, codeS]

class ToIntCodeOp t where
  toOp :: t -> IntCodeOp n

instance ToIntCodeOp Int where
  toOp x [mode] = do
    vm@IntcodeVM{..} <- lift get
    write x mode
    return Nothing
  toOp x modes = do
    vm <- lift get
    throwE $ "Leftover modes: " ++ show modes ++ "\n" ++ dumpVM vm

instance ToIntCodeOp r => ToIntCodeOp (Int -> r) where
  toOp f [] = do
    vm <- lift get
    throwE $ "Ran out of modes.\n" ++ dumpVM vm
  toOp f (m:ms) = do
    x <- readWithMode m
    toOp (f x) ms

wrapMaybe str = maybeToExceptT str . MaybeT . pure

readWithMode :: Mode -> Interpreter Int
readWithMode Positional = do
  vm@IntcodeVM{..} <- lift get
  addr <- wrapMaybe ("Attempt to access memory failed.\n" ++ dumpVM vm) $ IntMap.lookup instrPointer code
  val <- wrapMaybe ("Attempt to access memory failed.\n" ++ dumpVM vm) $ IntMap.lookup addr code
  lift $ put vm {instrPointer = succ instrPointer}
  return val
readWithMode Immediate = do
  vm@IntcodeVM{..} <- lift get
  val <- wrapMaybe ("Attempt to access memory failed.\n" ++ dumpVM vm) $ IntMap.lookup instrPointer code
  lift $ put vm {instrPointer = succ instrPointer}
  return val

write :: Int -> Mode -> Interpreter ()
write x Positional = do
  vm@IntcodeVM{..} <- lift get
  addr <- wrapMaybe ("Attempt to access memory failed.\n" ++ dumpVM vm) $ IntMap.lookup instrPointer code
  let newCode = IntMap.insert addr x code
      advPointer = succ instrPointer
  lift $ put vm {code = newCode, instrPointer = advPointer}
  return ()
write x Immediate = do
  vm <- lift get
  throwE $ "Attempt to write with immediate mode.\n" ++ dumpVM vm

inputOp :: IntCodeOpWithInput 1
inputOp = IntCodeOpWithInput inputOp'
  where
    inputOp' modes = do
      vm <- lift get
      throwE $ "Invalid input modes: " ++ show modes ++ "\n" ++ dumpVM vm

outputOp :: IntCodeOpWithInput 0
outputOp = NoInputs outputOp'
  where
    outputOp' [mode] = do
      x <- readWithMode mode
      return $ Just x
    outputOp' modes = do
      vm <- lift get
      throwE $ "Invalid output: " ++ show modes ++ "\n" ++ dumpVM vm

halt :: IntCodeOpWithInput 0
halt = IntCodeOpWithInput halt'
  where
    halt' [] = do
      vm <- lift get
      lift $ put vm {haltState = Halt}
      return Nothing
    halt' modes = do
      vm <- lift get
      throwE $ "Trying to halt with modes: " ++ show modes ++ "\n" ++ dumpVM vm

jumpConditional :: (Int -> Bool) -> IntCodeOpWithInput 0
jumpConditional f = IntCodeOpWithInput jumpIfTrue'
  where
    jumpIfTrue' [firstMode, secondMode] = do
      vm <- lift get
      check <- readWithMode firstMode
      jumpTo <- readWithMode secondMode
      if f check then lift $ put vm {instrPointer = jumpTo}
                 else pure ()
      return Nothing

writeConditional :: (Int -> Int -> Bool) -> IntCodeOpWithInput 0
writeConditional f = IntCodeOpWithInput writeConditional'
  where
    writeConditional' [firstMode,secondMode,thirdMode] = do
      first <- readWithMode firstMode
      second <- readWithMode secondMode
      write (fromEnum $ first `f` second) thirdMode
      return Nothing
    writeConditional' modes = do
      vm <- lift get
      throwE $ "Attempting conditional write with immediate mode: " ++ show modes ++ "\n" ++ dumpVM vm

ops :: IntMap (IntCodeOpWithInput n)
ops = IntMap.fromList
  [ (1, toIntcodeOp ((+) :: Int -> Int -> Int))
  , (2, toIntcodeOp ((*) :: Int -> Int -> Int))
  , (3, inputOp)
  , (4, outputOp)
  , (5, jumpConditional (/= 0))
  , (6, jumpConditional (== 0))
  , (7, writeConditional (<))
  , (8, writeConditional (==))
  , (99, halt)
  ]

modifyIntcode :: Code -> [(Int, Int)] -> Code
modifyIntcode code = Foldl.fold map
  where
    map = Fold step code id
    step m (k, v) = IntMap.insert k v m

parseOpCode :: Monad m => Int -> ExceptT String m (Int, [Mode])
parseOpCode n =
  let digitsRev = reverse . show $ n
      opCode = read @Int . reverse . take 2 $ digitsRev
      toMode '0' = pure Positional
      toMode '1' = pure Immediate
      toMode c = throwE $ "Invalid mode code: " ++ show c ++ " in " ++ show n ++ "\n"
      modes = traverse toMode . drop 2 $ digitsRev
   in sequenceA (opCode, modes)

runIntcodeOp :: Interpreter (Maybe Int)
runIntcodeOp = do
  vm <- lift get
  opCode <- readWithMode Immediate
  parseOp <-
    catchE
      (parseOpCode opCode)
      (\err -> throwE $ err ++ "\tDump: " ++ dumpVM vm ++ "\n")
  let (opCode, modes) = parseOp
  op <- wrapMaybe ("OpCode " ++ show opCode ++ " not recognised") $ IntMap.lookup opCode ops
  let modesPadded = modes ++ replicate (icopParams op - length modes) Positional
  icopRun op modesPadded

initVM :: Code -> [Input] -> IntcodeVM
initVM code inputs =
  let instrPointer = 0
      haltState = Run
   in IntcodeVM {..}

instrStream :: Stream ((->) Int) Identity (Maybe Int)
instrStream = Streaming.yields (const Nothing)

runIntcode :: IntcodeVM -> Stream (Of Int) Identity () -> Stream (Of Int) Interpreter ()
runIntcode vm inputStream = undefined

initAndRun = undefined

readIntCode :: Monad m => Text.Text -> ExceptT String m Code
readIntCode text =
  let codeOps = Text.splitOn "," text
      intCodeList = except $ traverse (fmap fst . Text.Read.signed Text.Read.decimal) codeOps
   in IntMap.fromList . zip [0..] <$> intCodeList

solve' :: ExceptT String IO ()
solve' = do
  codeText <- lift Text.IO.getContents
  intCode <- readIntCode codeText
  pure ()

solve :: IO ()
solve = do
  result <- runExceptT solve'
  case result of
    Left err -> putStrLn err
    Right () -> pure ()

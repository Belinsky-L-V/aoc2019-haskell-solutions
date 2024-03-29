{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Intcode
  ( Code
  , Halt(..)
  , Outcome(..)
  , IntCodeVM(..)
  , initVM
  , dummyVM
  , InputStream
  , OutputStream
  , OutputStreamM
  , wrapMaybe
  , readIntCode
  , modifyIntcode
  , runProgram
  , runIntCodeVM
  , runWithFeedback
  , runWithFeedbackM
  , rollbackFailure
  , module Control.Monad.Trans.Class
  , module Data.Functor.Identity
  , module Control.Monad.Trans.Except
  , module Streaming
  ) where

import Control.Foldl (Fold(..))
import qualified Control.Foldl as Foldl
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import Streaming
import qualified Streaming.Prelude as S
import Text.Read (readEither)

data Halt
  = Run
  | Halt
  deriving (Show, Read, Eq)

data Mode
  = Positional
  | Immediate
  | Relative
  deriving (Show, Read, Eq)

data Outcome
  = Success
  | UnkownOp String
  | OpNoParse String
  | ModeNoParse String
  | ReadFromEmpty String
  | SegFault String
  | InvalidModes String
  deriving (Show, Read, Eq)

type Code = IntMap Int
type Addr = Int
type Input = Int
type Output = Int
type InputStream = Stream (Of Int) Identity ()
type OutputStream = OutputStreamM Identity
type OutputStreamM m = Stream (Of Int) m (Outcome, IntCodeVM)
type Interpreter = ExceptT Outcome (State IntCodeVM)
type Op = [Mode] -> Interpreter (Maybe Int)

data IntCodeOp = IntCodeOp
  { intCodeOp :: Op
  , paramCount :: Int
  }

data IntCodeVM = IntCodeVM
  { haltState :: Halt
  , code :: Code
  , instrPointer :: Int
  , relBase :: Int
  , inputBuf :: InputStream
  }

dummyVM :: IntCodeVM
dummyVM = IntCodeVM Halt IntMap.empty (-1) (-1) (pure ())

initVM :: Code -> InputStream -> IntCodeVM
initVM code = IntCodeVM Run code 0 0

instance Show IntCodeVM where
  show vm@IntCodeVM{..} =
    let haltS = "Halt state: " ++ show haltState
        addrS = "At address: " ++ show instrPointer
        codeS = "With code: " ++ show code
        inputS = "With unprocessed inputs: " ++ show inputBuf
     in unlines [haltS, addrS, codeS, inputS]

class ToIntCodeOp t where
  countParams :: proxy t -> Int
  toOp :: t -> Op

instance ToIntCodeOp Int where
  countParams _ = 1
  toOp x [mode] = do
    vm@IntCodeVM{..} <- lift get
    write x mode
    return Nothing
  toOp x modes = do
    vm <- lift get
    throwE . InvalidModes $ "Leftover modes: " ++ show modes

instance ToIntCodeOp r => ToIntCodeOp (Int -> r) where
  countParams _ = 1 + countParams (Proxy @r)
  toOp f [] = throwE . InvalidModes $ "Ran out of modes"
  toOp f (m:ms) = do
    x <- readWithMode m
    toOp (f x) ms

toIntcodeOp :: forall f. ToIntCodeOp f => f -> IntCodeOp
toIntcodeOp f = IntCodeOp (toOp f) (countParams (Proxy @f))

wrapMaybe str = maybeToExceptT str . MaybeT . pure

memLookup :: Int -> Code -> Interpreter Int
memLookup at code =
  case IntMap.lookup at code of
    Nothing ->
      if at >= 0
        then pure 0
        else throwE . SegFault $ "Attempt to access memory at negative address."
    Just a -> pure a

readWithMode :: Mode -> Interpreter Int
readWithMode Positional = do
  vm@IntCodeVM{..} <- lift get
  addr <- memLookup instrPointer code
  val <- memLookup addr code
  lift $ put vm {instrPointer = succ instrPointer}
  return val
readWithMode Relative = do
  vm@IntCodeVM{..} <- lift get
  addr <- memLookup instrPointer code
  val <- memLookup (addr + relBase) code
  lift $ put vm {instrPointer = succ instrPointer}
  return val
readWithMode Immediate = do
  vm@IntCodeVM{..} <- lift get
  val <- memLookup instrPointer code
  lift $ put vm {instrPointer = succ instrPointer}
  return val

write :: Int -> Mode -> Interpreter ()
write x Positional = do
  vm@IntCodeVM{..} <- lift get
  addr <- memLookup instrPointer code
  let newCode = IntMap.insert addr x code
      advPointer = succ instrPointer
  lift $ put vm {code = newCode, instrPointer = advPointer}
  return ()
write x Relative = do
  vm@IntCodeVM{..} <- lift get
  addr <- memLookup instrPointer code
  let newCode = IntMap.insert (addr + relBase) x code
      advPointer = succ instrPointer
  lift $ put vm {code = newCode, instrPointer = advPointer}
  return ()
write x Immediate = throwE . InvalidModes $ "Attempt to write with immediate mode."

acceptInput :: Interpreter Int
acceptInput = do
  vm@IntCodeVM {..} <- lift get
  (x, restOfInput) <-
    wrapMaybe (ReadFromEmpty "Attemping to get input from an empty buffer.") $
    runIdentity (S.uncons inputBuf)
  lift $ put vm {inputBuf = restOfInput}
  return x

inputOp :: IntCodeOp
inputOp = IntCodeOp inputOp' 1
  where
    inputOp' [mode] = do
      x <- acceptInput
      write x mode
      return Nothing
    inputOp' modes = do
      vm <- lift get
      throwE . InvalidModes $ "Invalid input modes: " ++ show modes

outputOp :: IntCodeOp
outputOp = IntCodeOp outputOp' 1
  where
    outputOp' [mode] = do
      x <- readWithMode mode
      return $ Just x
    outputOp' modes = do
      vm <- lift get
      throwE . InvalidModes $ "Invalid output modes: " ++ show modes

halt :: IntCodeOp
halt = IntCodeOp halt' 0
  where
    halt' [] = do
      vm <- lift get
      lift $ put vm {haltState = Halt}
      return Nothing
    halt' modes = do
      vm <- lift get
      throwE . InvalidModes $ "Trying to halt with modes: " ++ show modes

jumpConditional :: (Int -> Bool) -> IntCodeOp
jumpConditional f = IntCodeOp jumpIfTrue' 2
  where
    jumpIfTrue' [firstMode, secondMode] = do
      vm <- lift get
      check <- readWithMode firstMode
      jumpTo <- readWithMode secondMode
      if f check
        then lift $ put vm {instrPointer = jumpTo}
        else pure ()
      return Nothing
    jumpIfTrue' modes = do
      vm <- lift get
      throwE . InvalidModes $
        "Conditional jump with invalid modes: " ++ show modes

writeConditional :: (Int -> Int -> Bool) -> IntCodeOp
writeConditional f = IntCodeOp writeConditional' 3
  where
    writeConditional' [firstMode, secondMode, thirdMode] = do
      first <- readWithMode firstMode
      second <- readWithMode secondMode
      write (fromEnum $ first `f` second) thirdMode
      return Nothing
    writeConditional' modes = do
      vm <- lift get
      throwE . InvalidModes $
        "Attempting conditional write with immediate mode: " ++ show modes

adjustRelBase :: IntCodeOp
adjustRelBase = IntCodeOp adjustRelBase' 1
  where
    adjustRelBase' [mode] = do
      adjBy <- readWithMode mode
      vm@IntCodeVM {..} <- lift get
      lift $ put vm {relBase = relBase + adjBy}
      return Nothing
    adjustRelBase' modes =
      throwE . InvalidModes $
      "Attempting relative base adjustment with invalid mdoes: " ++ show modes

ops :: IntMap IntCodeOp
ops = IntMap.fromList
  [ (1, toIntcodeOp ((+) :: Int -> Int -> Int))
  , (2, toIntcodeOp ((*) :: Int -> Int -> Int))
  , (3, inputOp)
  , (4, outputOp)
  , (5, jumpConditional (/= 0))
  , (6, jumpConditional (== 0))
  , (7, writeConditional (<))
  , (8, writeConditional (==))
  , (9, adjustRelBase)
  , (99, halt)
  ]

modifyIntcode :: Code -> [(Int, Int)] -> Code
modifyIntcode code = Foldl.fold map
  where
    map = Fold step code id
    step m (k, v) = IntMap.insert k v m

parseOpCode :: Monad m => Int -> ExceptT Outcome m (Int, [Mode])
parseOpCode n = do
  let digitsRev = reverse . show $ n
  opCode <- withExceptT OpNoParse . except $ readEither . reverse . take 2 $ digitsRev
  let toMode '0' = pure Positional
      toMode '1' = pure Immediate
      toMode '2' = pure Relative
      toMode c = throwE . ModeNoParse $ "Invalid mode code: " ++ show c ++ " in " ++ show n
  modes <- traverse toMode . drop 2 $ digitsRev
  return (opCode, modes)

runIntcodeOp :: Interpreter (Maybe Int)
runIntcodeOp = do
  vm <- lift get
  opCode <- readWithMode Immediate
  parseOp <- parseOpCode opCode
  let (opCode, modes) = parseOp
  op <-
    wrapMaybe (UnkownOp $ "OpCode " ++ show opCode ++ " not recognised") $
    IntMap.lookup opCode ops
  let modesPadded = modes ++ replicate (paramCount op - length modes) Positional
  intCodeOp op modesPadded

runProgram :: Monad m => Code -> InputStream -> OutputStreamM m
runProgram code inputs = runIntCodeVM (initVM code inputs)

runIntCodeVM :: Monad m => IntCodeVM -> OutputStreamM m
runIntCodeVM vm = hoist (return . runIdentity) $ repackResult <$> finalStream vm
  where
    oneOp :: Interpreter (Either (Maybe Int) ())
    oneOp = do
      vm@IntCodeVM {haltState} <- lift get
      case haltState of
        Halt -> return $ Right ()
        Run -> Left <$> runIntcodeOp
    opStream :: Stream (Of Int) Interpreter ()
    opStream = S.catMaybes $ S.untilRight oneOp
    stateStream = runExceptT (distribute opStream)
    finalStream = runStateT (distribute stateStream)
    repackResult :: (Either Outcome (), IntCodeVM) -> (Outcome, IntCodeVM)
    repackResult (Left err, vm) = (err, vm)
    repackResult (Right (), vm) = (Success, vm)

runWithFeedback ::
     ([Int] -> InputStream) -> IntCodeVM -> ([Int], (Outcome, IntCodeVM))
runWithFeedback feedback vm =
  runIdentity (runWithFeedbackM (return . feedback) vm)

runWithFeedbackM ::
     Monad m
  => ([Int] -> m InputStream)
  -> IntCodeVM
  -> m ([Int], (Outcome, IntCodeVM))
runWithFeedbackM feedback vm = do
  result@(outputList, (state, vm@IntCodeVM {..})) <-
    S.lazily <$> S.toList (runIntCodeVM vm)
  case state of
    ReadFromEmpty _ -> do
      newInput <- feedback outputList
      let newVM = vm {instrPointer = instrPointer - 1, inputBuf = newInput}
      runWithFeedbackM feedback newVM
    _ -> return result

rollbackFailure :: (Outcome, IntCodeVM) -> Maybe IntCodeVM
rollbackFailure (Success, vm) = Just vm
rollbackFailure (OpNoParse _, vm@IntCodeVM {..}) =
  Just $ vm {instrPointer = instrPointer - 1}
rollbackFailure (UnkownOp _, vm@IntCodeVM {..}) =
  Just $ vm {instrPointer = instrPointer - 1}
rollbackFailure (ModeNoParse _, vm@IntCodeVM {..}) =
  Just $ vm {instrPointer = instrPointer - 1}
rollbackFailure (ReadFromEmpty _, vm@IntCodeVM {..}) =
  Just $ vm {instrPointer = instrPointer - 1}
rollbackFailure (_, _) = Nothing

readIntCode :: Monad m => Text.Text -> ExceptT String m Code
readIntCode text =
  let codeOps = Text.splitOn "," text
      intCodeList = except $ traverse (fmap fst . Text.Read.signed Text.Read.decimal) codeOps
   in IntMap.fromList . zip [0..] <$> intCodeList

{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleInstances,
  TypeApplications, ScopedTypeVariables, NamedFieldPuns,
  RecordWildCards #-}
module Day5 where

import Control.Monad.ST
import Data.STRef
import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import Data.Vector (Vector, MVector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Data.Proxy
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data Halt = Run | Halt deriving (Show, Read, Eq)
data Mode = Pos | Immed deriving (Show, Read, Eq)

type Code s = MVector s Int
type Addr = Int
type Input = Int
type Output = Int
type Op s = [Mode] -> Interpreter s ()

data IntcodeOp = IntcodeOp
  { icopRun :: forall s. Op s
  , icopParams :: !Int
  }

data IntcodeVM s = IntcodeVM
  { halted :: STRef s Halt
  , code :: Code s
  , inputBuf :: STRef s [Input]
  , outputBuf :: STRef s [Output]
  , instPointer :: STRef s Int
  }

dumpVM :: forall s. IntcodeVM s -> ST s String
dumpVM vm@IntcodeVM{..} = do
  halt <- readSTRef halted
  code <- Vector.freeze code
  input <- readSTRef inputBuf
  output <- readSTRef outputBuf
  pointer <- readSTRef instPointer
  return $ unlines [show (halt, pointer), show (input, output), show code]

type Interpreter s = ReaderT (IntcodeVM s) (ExceptT String (ST s))

class ToIntcodeOp t where
  params_ :: proxy t -> Int
  toOp_ :: t -> forall s. Op s

instance ToIntcodeOp Int where
  params_ _ = 1
  toOp_ x [Pos] = do
    vm@IntcodeVM{..} <- ask
    targetAddr <- withMode Immed
    MVector.write code targetAddr x
    return ()
  toOp_ x modes = ReaderT $ \vm@IntcodeVM{..} -> do
    dump <- lift $ dumpVM vm
    throwE $ "Mode for writing isn't positional, or leftover modes: " ++ show modes ++ " " ++ dump

instance ToIntcodeOp r => ToIntcodeOp (Int -> r) where
  params_ _ = 1 + params_ (Proxy @r)
  toOp_ f [] = ReaderT $ \vm -> do
    dump <- lift $ dumpVM vm
    throwE $ "Ran out of modes: " ++ dump
  toOp_ f (m:ms) = do
    x <- withMode m
    toOp_ (f x) ms

toIntcodeOp :: forall f. ToIntcodeOp f => f -> IntcodeOp
toIntcodeOp f = IntcodeOp (toOp_ f) (params_ (Proxy @f))

withMode :: Mode -> Interpreter s Int
withMode Pos = ReaderT $ \vm@IntcodeVM{..} -> do
  arg <- lift $ MVector.read code =<< MVector.read code =<< readSTRef instPointer
  lift $ modifySTRef instPointer succ
  return arg
withMode Immed = ReaderT $ \vm@IntcodeVM{..} -> do
  arg <- lift $ MVector.read code =<< readSTRef instPointer
  lift $ modifySTRef instPointer succ
  return arg

inputOp :: IntcodeOp
inputOp = IntcodeOp inputOp' 1
  where
    inputOp' :: forall s. Op s
    inputOp' [Pos] = do
      vm@IntcodeVM{inputBuf, code} <- ask
      (x:rest) <- lift . lift $ readSTRef inputBuf
      writeAddr <- withMode Immed
      MVector.write code writeAddr x
      lift . lift $ modifySTRef inputBuf tail
      return ()
    inputOp' modes = ReaderT $ \vm -> do
      dump <- lift $ dumpVM vm
      throwE $ "Invalid input modes: " ++ show modes ++ " " ++ dump

outputOp :: IntcodeOp
outputOp = IntcodeOp outputOp' 1
  where
    outputOp' :: forall s. Op s
    outputOp' [mode] = do
      vm@IntcodeVM{outputBuf} <- ask
      x <- withMode mode
      lift . lift $ modifySTRef outputBuf (x:)
    outputOp' modes = ReaderT $ \vm -> do
      dump <- lift $ dumpVM vm
      throwE $ "Invalid output: " ++ show modes ++ " " ++ dump

halt :: IntcodeOp
halt = IntcodeOp halt' 0
  where
    halt' [] = ReaderT $ \IntcodeVM{halted} -> lift $ writeSTRef halted Halt
    halt' modes = ReaderT $ \vm -> do
      dump <- lift $ dumpVM vm
      throwE $ "Trying to halt with modes: " ++ show modes ++ " " ++ dump

jumpConditional :: (Int -> Bool) -> IntcodeOp
jumpConditional f = IntcodeOp jumpIfTrue' 2
  where
    jumpIfTrue' :: forall s. Op s
    jumpIfTrue' [firstMode, secondMode] = do
      vm@IntcodeVM{..} <- ask
      check <- withMode firstMode
      jumpTo <- withMode secondMode
      when (f check) . lift . lift $ writeSTRef instPointer jumpTo

writeConditional :: (Int -> Int -> Bool) -> IntcodeOp
writeConditional f = IntcodeOp writeConditional' 3
  where
    writeConditional' :: forall s. Op s
    writeConditional' [firstMode,secondMode,Pos] = do
      vm@IntcodeVM{..} <- ask
      first <- withMode firstMode
      second <- withMode secondMode
      writeTo <- withMode Immed
      MVector.write code writeTo (fromEnum $ first `f` second)
    writeConditional' modes = ReaderT $ \vm -> do
      dump <- lift $ dumpVM vm
      throwE $ "Attempting conditional write with immediate mode: " ++ show modes ++ " " ++ dump

ops :: IntMap IntcodeOp
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

modifyIntcode :: Vector Int -> [(Int, Int)] -> ST s (Code s)
modifyIntcode vec subs = do
  mutvec <- Vector.thaw vec
  mapM_ (uncurry (MVector.write mutvec)) subs
  return mutvec

parseOpCode :: Monad m => Int -> ExceptT String m (Int, [Mode])
parseOpCode n =
  let digitsRev = reverse . show $ n
      opCode = read @Int . reverse . take 2 $ digitsRev
      toMode '0' = pure Pos
      toMode '1' = pure Immed
      toMode c = throwE $ "Invalid mode code: " ++ show c ++ " in " ++ show n ++ "\n"
      modes = traverse toMode . drop 2 $ digitsRev
   in sequenceA (opCode, modes)

runIntcodeOp :: Interpreter s ()
runIntcodeOp = do
  vm <- ask
  opCode <- withMode Immed
  parseOp <-
    lift $
    catchE
      (parseOpCode opCode)
      (\err -> do
         dump <- lift $ dumpVM vm
         throwE $ err ++ "\tDump: " ++ dump ++ "\n")
  let (opCode, modes) = parseOp
  op <-
    lift $
    maybeToExceptT ("OpCode " ++ show opCode ++ " not recognised") .
    MaybeT . pure $
    IntMap.lookup opCode ops
  let modesPadded = modes ++ replicate (icopParams op - length modes) Pos
  icopRun op modesPadded

initVM :: (forall s. (ST s) (Code s)) -> [Input] -> ST s (IntcodeVM s)
initVM mcode inputs = do
  instPointer <- newSTRef 0
  inputBuf <- newSTRef inputs
  outputBuf <- newSTRef []
  halted <- newSTRef Run
  code <- mcode
  return $ IntcodeVM {..}

runIntcode :: forall s. Interpreter s (Addr, [Input], [Output], Vector Int)
runIntcode = do
  run
  vm@IntcodeVM{..} <- ask
  finalPtr <- lift. lift $ readSTRef instPointer
  finalCode <- Vector.freeze code
  finalInput <- lift . lift $ readSTRef inputBuf
  finalOutput <- lift. lift $ readSTRef outputBuf
  return (finalPtr, finalInput, finalOutput, finalCode)
    where
      run = do
        vm@IntcodeVM{..} <- ask
        haltedNow <- lift . lift $ readSTRef halted
        currentPtr <- lift . lift $ readSTRef instPointer
        unless (haltedNow == Halt) $
          if haltedNow == Run && (currentPtr < 0 || currentPtr >= MVector.length code)
             then lift . throwE $ "Attempting to access instruction out of memory at: " ++ show currentPtr
             else runIntcodeOp >> run

initAndRun :: Monad m => (forall s. (ST s) (Code s)) -> [Input] -> ExceptT String m (Addr, [Input], [Output], Vector Int)
initAndRun mcode inputs = except $ runST (runExceptT go)
  where
    go = do
      vm <- lift $ initVM mcode inputs
      runReaderT runIntcode vm

readIntCode :: Monad m => Text.Text -> ExceptT String m (Vector Int)
readIntCode text =
  let codeOps = Text.splitOn "," text
      intCodeList = except $ traverse (fmap fst . Text.Read.signed Text.Read.decimal) codeOps
   in Vector.fromList <$> intCodeList

solve' :: ExceptT String IO ()
solve' = do
  codeText <- lift Text.IO.getContents
  intCodeVec <- readIntCode codeText
  (ptr, input, output, code) <- initAndRun (modifyIntcode intCodeVec []) [1]
  lift . print $ reverse output
  (ptr, input, output, code) <- initAndRun (modifyIntcode intCodeVec []) [5]
  lift . print $ reverse output
  pure ()

solve :: IO ()
solve = do
  result <- runExceptT solve'
  case result of
    Left err -> putStrLn err
    Right () -> pure ()

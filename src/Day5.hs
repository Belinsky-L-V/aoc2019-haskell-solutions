{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleInstances, TypeApplications, ScopedTypeVariables #-}
module Day5 where

import Data.Functor.Identity
import Control.Monad.ST
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe
import Data.Either (rights)
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe)
import Data.Foldable (asum)
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
import Debug.Trace (trace)
import Control.Monad.Trans.Except

data Halt = Run | Halt deriving (Show, Read, Eq)
data Mode = Pos | Immed deriving (Show, Read, Eq)

type Code s = MVector s Int
type Addr = Int
type Input = Int
type Output = Int
type Op s = Addr -> [Mode] -> Code s -> [Input] -> ExceptT String (ST s) ([Input], [Output])

data IntcodeOp = IntcodeOp
  { icopRun :: forall s. Op s
  , icopArity :: !Int
  }

class ToIntcodeOp t where
  arity_ :: proxy t -> Int
  toOp_ :: t -> forall s. Op s

instance ToIntcodeOp Int where
  arity_ _ = 0
  toOp_ x addr [Pos] code input = do
    targetAddr <- MVector.read code addr
    MVector.write code targetAddr x
    return (input, [])
  toOp_ x addr modes code input = throwE $ "At " ++ show addr ++ ";\n Mode for writing isn't positional, or leftover modes: " ++ show modes

instance ToIntcodeOp r => ToIntcodeOp (Int -> r) where
  arity_ _ = 1 + arity_ (Proxy @r)
  toOp_ f addr [] code input = throwE $ "At " ++ show addr ++ ";\n Ran out of modes"
  toOp_ f addr (m:ms) code input = do
    x <- case m of
           Pos -> do
             readAddr <- MVector.read code addr
             MVector.read code readAddr
           Immed -> MVector.read code addr
    toOp_ (f x) (succ addr) ms code input

toIntcodeOp :: forall f. ToIntcodeOp f => f -> IntcodeOp
toIntcodeOp f = IntcodeOp (toOp_ f) (arity_ (Proxy @f))

inputOp :: IntcodeOp
inputOp = IntcodeOp inputOp' 0
  where
    inputOp' :: forall s. Op s
    inputOp' addr [Pos] code (x:input) = do
      writeAddr <- MVector.read code addr
      MVector.write code writeAddr x
      return (input, [])
    inputOp' addr modes code input = throwE $ "Invalid input: " ++ show (addr, modes, input)

outputOp :: IntcodeOp
outputOp = IntcodeOp outputOp' 0
  where
    outputOp' :: forall s. Op s
    outputOp' addr [Pos] code input = do
      x <- MVector.read code =<< MVector.read code addr
      return (input, [x])
    outputOp' addr [Immed] code input = do
      x <- MVector.read code addr
      return (input, [x])
    outputOp' addr modes code input = throwE $ "Invalid output: " ++ show (addr, modes, input)

halt :: IntcodeOp
halt = IntcodeOp halt' 0
  where
    halt' addr modes code input = pure (input, [])

ops :: IntMap IntcodeOp
ops = IntMap.fromList
  [ (1, toIntcodeOp ((+) :: Int -> Int -> Int))
  , (2, toIntcodeOp ((*) :: Int -> Int -> Int))
  , (3, inputOp)
  , (4, outputOp)
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
      toMode c = throwE $ "Invalid mode code: " ++ show c ++ " in " ++ show n
      modes = traverse toMode . drop 2 $ digitsRev
   in sequenceA (opCode, modes)

runIntcodeOp :: Int -> Code s -> [Int] -> ExceptT String (ST s) (Halt, Addr, [Input], [Output])
runIntcodeOp addr code input = do
  parseOp <- parseOpCode <$> MVector.read code addr
  (opCode, modes) <- parseOp
  op <- maybeToExceptT ("OpCode " ++ show opCode ++ " not recognised") . MaybeT . pure $ IntMap.lookup opCode ops
  let modesPadded = modes ++ replicate (icopArity op + 1 - length modes) Pos
  (newInput, output) <- icopRun op (addr + 1)  modesPadded code input
  let nextAddr = addr + icopArity op + 2
  return (if opCode == 99 then Halt else Run, nextAddr, newInput, output)


runIntcode :: (forall s. ST s (Code s)) -> [Int] -> Either String (([Int], [Int]), Vector Int)
runIntcode mutvec inputs = runST $ runIntcode' mutvec inputs
  where
    runIntcode' :: (forall s. ST s (Code s)) -> [Int] -> forall s. (ST s) (Either String (([Int], [Int]), Vector Int))
    runIntcode' mutvec inputs = runExceptT $ do
      code <- lift mutvec
      finalResult <- run 0 code inputs
      finalCode <- Vector.freeze code
      return (finalResult, finalCode)
        where
          run addr code inputs = do
            (state, nextAddr, currentInput, currentOutput) <- runIntcodeOp addr code inputs
            if state == Run && (nextAddr < 0 || nextAddr >= MVector.length code)
               then throwE $ "Attempting to access instruction out of memory at: " ++ show nextAddr
               else case state of
                      Halt -> return (currentInput, currentOutput)
                      Run -> do
                        (newInput, newOutput) <- run nextAddr code currentInput
                        return (newInput, currentOutput ++ newOutput)

readIntCode :: Text.Text -> Except String (Vector Int)
readIntCode text =
  let codeOps = Text.splitOn "," text
      intCodeList = except $ traverse (fmap fst . Text.Read.signed Text.Read.decimal) codeOps
   in Vector.fromList <$> intCodeList

solve :: IO ()
solve = do
  codeText <- Text.IO.getContents
  let intCodeVec' = readIntCode codeText
  case runExcept intCodeVec' of
    Left err -> print $ "Failed input parse: " ++ err
    Right intCodeVec -> print $ runIntcode (modifyIntcode intCodeVec []) [1]

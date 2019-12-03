{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleInstances, TypeApplications, ScopedTypeVariables #-}
module Day2 (solve) where

import Control.Monad.ST
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Either (rights)
import Control.Monad (when, unless)
import Data.Vector (Vector, MVector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Mutable as MVector
import Data.Proxy
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

type Code s = MVector s Int

data IntcodeOp = IntcodeOp
  { icopRun :: Int -> forall s. Code s -> ST s Int
  , icopArity :: !Int
  }

class ToIntcodeOp t where
  arity_ :: proxy t -> Int
  toOp_ :: t -> forall s. Int -> Code s -> ST s Int

instance ToIntcodeOp Int where
  arity_ _ = 0
  toOp_ x _ _ = pure x

instance ToIntcodeOp r => ToIntcodeOp (Int -> r) where
  arity_ _ = 1 + arity_ (Proxy @r)
  toOp_ f idx vec = do
    addr <- MVector.read vec idx
    x <- MVector.read vec addr
    toOp_ (f x) (succ idx) vec

toIntcodeOp :: forall f. ToIntcodeOp f => f -> IntcodeOp
toIntcodeOp f = IntcodeOp (toOp_ f) (arity_ (Proxy @f))

ops :: IntMap IntcodeOp
ops = IntMap.fromList
  [ (1, toIntcodeOp ((+) :: Int -> Int -> Int))
  , (2, toIntcodeOp ((*) :: Int -> Int -> Int))
  ]

modifyIntcode :: Vector Int -> [(Int, Int)] -> ST s (Code s)
modifyIntcode vec subs = do
  mutvec <- Vector.thaw vec
  mapM_ (uncurry (MVector.write mutvec)) subs
  return mutvec

runIntcode :: (forall s. ST s (Code s)) -> Vector Int
runIntcode mutvec = runST $ do
  code <- mutvec
  runIntcode' 0 code
  Vector.freeze code
  where
    runIntcode' :: Int -> Code s -> ST s ()
    runIntcode' idx code = do
      opCode <- MVector.read code idx
      case IntMap.lookup opCode ops of
        Nothing -> return ()
        Just op -> do
          targetAddr <- MVector.read code (idx + icopArity op + 1)
          result <- icopRun op (idx + 1) code
          MVector.write code targetAddr result
          runIntcode' (idx + icopArity op + 2) code

readIntCode :: IO (Vector Int)
readIntCode = do
  textList <- TIO.getContents
  let codeOps = T.splitOn "," textList
      intCodeList = map fst . rights . map TR.decimal $ codeOps
  return $ Vector.fromList intCodeList

solve :: IO ()
solve = do
  let changes = [(1, 12), (2, 2)]
  intCodeVec <- readIntCode
  print . Vector.head $ runIntcode (modifyIntcode intCodeVec changes)
  let results =
        [ (noun, verb)
        | noun <- [1 .. 99]
        , verb <- [1 .. 99]
        , 19690720 == Vector.head (runIntcode (modifyIntcode intCodeVec [(1, noun), (2, verb)]))
        ]
  unless
    (null results)
    (let (noun, verb) = head results
      in print $ 100 * noun + verb)

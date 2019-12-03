{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleInstances, TypeApplications, ScopedTypeVariables #-}
module Day2 where

import Control.Monad.ST
import Data.STRef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Either (rights)
import Control.Monad (when)
import Data.Vector (MVector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MVector
import Data.Proxy


data IntcodeOp = IntcodeOp
  { icopRun :: forall s. MVector s Int -> ST s Int
  , icopArity :: !Int
  }

class ToIntcodeOp t where
  arity_ :: proxy t -> Int
  toOp_ :: t -> forall s. MVector s Int -> ST s Int

instance ToIntcodeOp Int where
  arity_ _ = 0
  toOp_ x _ = pure x

instance ToIntcodeOp r => ToIntcodeOp (Int -> r) where
  arity_ _ = 1 + arity_ (Proxy @r)
  toOp_ f vec = do
    x <- MVector.read vec 0
    toOp_ (f x) (MVector.drop 1 vec)

toIntcodeOp :: forall f. ToIntcodeOp f => f -> IntcodeOp
toIntcodeOp f = IntcodeOp (toOp_ f) (arity_ (Proxy @f))

type Code s = MVector s Int

runIntcode :: [Int] -> Int
runIntcode lst = runST $ do
  code <- V.thaw $ V.fromList lst
  MVector.write code 1 12
  MVector.write code 2 2
  runIntcode' 0 code
  MVector.read code 0
  where
    runIntcode' :: Int -> Code s -> ST s ()
    runIntcode' idx code = do
      op <- MVector.read code idx
      when (op == 1 || op == 2) $ do
          [addr1,addr2,addro] <- sequenceA $ (\o -> MVector.read code (idx + o)) <$> [1..3]
          arg1 <- MVector.read code addr1
          arg2 <- MVector.read code addr2
          if op == 1
             then MVector.write code addro (arg1 + arg2)
             else MVector.write code addro (arg1 * arg2)
          runIntcode' (idx + 4) code

part1 :: IO ()
part1 = do
  textList <- TIO.getContents
  let codeOps = T.splitOn "," textList
      intCodeList = map fst . rights . map TR.decimal $ codeOps
  print $ runIntcode intCodeList

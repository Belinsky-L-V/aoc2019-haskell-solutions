module Day10LawTest
  ( repOrdTest
  ) where

import Data.Proxy (Proxy(..))
import Day10 (Rep, rep)
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary Rep where
  arbitrary = do
    x <- arbitrary :: Gen Int
    y <- arbitrary :: Gen Int
    return $ rep (x, y)

repOrd = ordLaws (Proxy :: Proxy Rep)

repOrdTest :: TestTree
repOrdTest =
  testProperties "Day10 Representative Ord" $
  fmap (withMaxSuccess 10000) <$> lawsProperties repOrd

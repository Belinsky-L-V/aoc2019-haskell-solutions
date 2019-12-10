module Tests where

import Test.Tasty
import Test.Tasty.Golden as Golden


main = defaultMain tests

tests :: TestTree
tests = testGroup "Correct solutions" []

-- Golden.goldenVsFile "day1" "test/golden/output1"

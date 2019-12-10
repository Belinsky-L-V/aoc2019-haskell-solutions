module Main where

import AllDays
import System.IO
import System.IO.Silently
import Test.Tasty
import Test.Tasty.Golden as Golden

main = defaultMain tests

tests :: TestTree
tests = testGroup "Correct solutions" $ testDay <$> allSolutions

allSolutions =
  [ (1, solve1)
  , (2, solve2)
  , (3, solve3)
  , (4, const solve4)
  , (5, solve5)
  , (6, solve6)
  , (7, solve7)
  , (8, solve8)
  , (9, solve9)
  ]

testDay :: (Int, Handle -> IO String) -> TestTree
testDay (n, f) =
  let correct = "test/golden/output" ++ show n
      name = "day" ++ show n
      inputFile = "inputs/input" ++ show n
      outputFile = "test/golden/testResult" ++ show n
      outputHandle = openFile outputFile WriteMode
      check = do
        inputHandle <- openFile inputFile ReadMode
        result <- f inputHandle
        outputHandle <- openFile outputFile WriteMode
        hPutStr outputHandle result
        hClose inputHandle
        hClose outputHandle
   in goldenVsFile name correct outputFile check

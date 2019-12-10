module Day1 (solve1) where

import System.IO (Handle, hGetContents)

fuel :: Int -> Int
fuel n = quot n 3 - 2

fuelTotal :: Int -> Int
fuelTotal n
  | f > 0 = f + fuelTotal f
  | otherwise = 0
    where
      f = fuel n

solve1 :: Handle -> IO String
solve1 handle = do
  input <- map read . lines <$> hGetContents handle
  return $ unlines [show (sum $ map fuel input), show (sum $ map fuelTotal input)]

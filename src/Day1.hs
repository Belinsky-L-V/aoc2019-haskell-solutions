module Day1 (solve) where

import System.IO (Handle, hGetContents)

fuel :: Int -> Int
fuel n = quot n 3 - 2

fuelTotal :: Int -> Int
fuelTotal n
  | f > 0 = f + fuelTotal f
  | otherwise = 0
    where
      f = fuel n

solve :: Handle -> IO ()
solve handle = do
  input <- map read . lines <$> hGetContents handle
  print . sum $ map fuel input
  print . sum $ map fuelTotal input

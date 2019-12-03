module Day1 (solve) where

fuel :: Int -> Int
fuel n = quot n 3 - 2

fuelTotal :: Int -> Int
fuelTotal n
  | f > 0 = f + fuelTotal f
  | otherwise = 0
    where
      f = fuel n

solve :: IO ()
solve = do
  input <- map read . lines <$> getContents
  print . sum $ map fuel input
  print . sum $ map fuelTotal input

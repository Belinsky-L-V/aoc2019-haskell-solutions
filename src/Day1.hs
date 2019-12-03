module Day1 where

fuel :: Int -> Int
fuel n = quot n 3 - 2

fuelTotal :: Int -> Int
fuelTotal n
  | f > 0 = f + fuelTotal f
  | otherwise = 0
    where
      f = fuel n

part1 :: IO ()
part1 = do
  total <- sum . map (fuel . read) . lines <$> getContents
  print total

part2 :: IO ()
part2 = do
  total <- sum . map (fuelTotal . read) . lines <$> getContents
  print total

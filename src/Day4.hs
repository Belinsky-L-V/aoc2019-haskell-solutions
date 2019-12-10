module Day4 (solve4) where

import Data.List (group, sort)

has2 n = elem 2 $ length <$> group (show n)
has2plus n = or $ (>=2) . length <$> group (show n)
nondec n = show n == sort (show n)

valid :: Int -> Int -> [Int -> Bool] -> [Int]
valid from to filters =
  let range = [from .. to]
   in filter (and . (filters <*>) . pure) range

solve4 :: IO String
solve4 = do
  let part1 = show $ length (valid 245182 790572 [nondec, has2plus])
      part2 = show $ length (valid 245182 790572 [nondec, has2plus, has2])
  return $ unlines [part1, part2]

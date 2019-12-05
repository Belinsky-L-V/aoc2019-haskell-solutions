{-# LANGUAGE TypeApplications #-}

module Day4 (solve) where

import Data.List (group, sort)

has2 n = elem 2 $ length <$> group (show n)
has2plus n = or $ (>=2) . length <$> group (show n)
nondec n = show n == sort (show n)

valid :: Int -> Int -> [Int -> Bool] -> [Int]
valid from to filters =
  let range = [from .. to]
   in filter (and . (filters <*>) . pure) range

solve :: IO ()
solve = do
  print $ length (valid 245182 790572 [nondec, has2plus])
  print $ length (valid 245182 790572 [nondec, has2plus, has2])

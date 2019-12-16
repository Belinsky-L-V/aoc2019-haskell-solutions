{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Day16
  ( solve16
  ) where

import Control.DeepSeq
import Data.Vector (Vector, (!))
import qualified Data.Vector as Vector
import System.IO (Handle, hGetContents)
import Text.ParserCombinators.ReadP

takeSums :: Vector Int -> Int -> Int -> Int
takeSums prefixSums n p =
  let posStartAt = p - 1
      negStartAt = posStartAt + p * 2
      fromTo startAt =
        Vector.fromList
          [ if s + p <= n
            then (s, s + p)
            else (s, n)
          | s <- [startAt,(startAt + 4 * p) .. n]
          ]
      findSum (f, t) = prefixSums ! t - prefixSums ! f
      sumUp = Vector.sum . Vector.map findSum . fromTo
   in (`mod` 10) . abs $ sumUp posStartAt - sumUp negStartAt

roundFTT :: Vector Int -> Vector Int
roundFTT inputDigits =
  let n = Vector.length inputDigits
      ps = Vector.enumFromN 1 n
      prefixSums = Vector.scanl' (+) 0 inputDigits
   in Vector.map (takeSums prefixSums n) ps

nRounds :: Int -> Vector Int -> Vector Int
nRounds n = snd . until ((== n) . fst) go . (0, )
  where
    go (force -> (c, ds)) = (succ c, roundFTT ds)

part1 :: Vector Int -> String
part1 ds =
  let after100 = nRounds 100 ds
      first8 = Vector.take 8 after100
   in concatMap show first8

part2 :: Vector Int -> String
part2 ds =
  let offsetDs = Vector.toList $ Vector.take 7 ds
      offset :: Int
      offset = read $ concatMap show offsetDs
      fullMessage = Vector.concat $ replicate 10000 ds
      after100 = nRounds 100 fullMessage
      messageV = Vector.take 8 . Vector.drop offset $ after100
   in concatMap show messageV

toDigits :: String -> Vector Int
toDigits input =
  let digit :: ReadP Int
      digit = read . pure <$> satisfy (`elem` ['0' .. '9'])
      allDs = many digit <* (char '\n' *> eof)
   in case readP_to_S allDs input of
        [(ds, _)] -> Vector.fromList ds
        _ -> Vector.empty

solve16 :: Handle -> IO String
solve16 handle = do
  input <- hGetContents handle
  let digits = toDigits input
  let part1out = part1 digits
  let part2out = part2 digits
  return . unlines $ [part1out, part2out]

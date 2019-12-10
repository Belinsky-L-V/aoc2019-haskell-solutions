module Day8 where

import Data.Maybe (fromMaybe)
import Streaming
import qualified Streaming.Prelude as S

part1 :: Monad m => Stream (Of Int) m r -> m (Of (Int,Int,Int) r)
part1 ints = do
  let layers = chunksOf (25 * 6) ints
      check (z,one,two) 0 = (succ z,one,two)
      check (z,one,two) 1 = (z,succ one,two)
      check (z,one,two) 2 = (z,one,succ two)
      check (z,one,two) _ = (z,one,two)
      checkLayer = S.fold check (0,0,0) id
      checked = mapped checkLayer layers
  first (fromMaybe (-1,-1,-1)) <$> S.minimum checked

part2 :: Monad m => Stream (Of Int) m r -> m (Of [Int] r)
part2 ints = do
  let layers = chunksOf (25 * 6) ints
      occludePixel 2 n = n
      occludePixel n _ = n
      occludeLayer = zipWith occludePixel
      listLayers = mapped S.toList layers
  S.fold occludeLayer (repeat 2) id listLayers

solve :: IO ()
solve = do
  let ints :: Stream (Of Int) IO ()
      ints = S.read . S.map pure . S.concat $ S.stdinLn
  result <- part2 $ S.store part1 ints
  let (pixels, p2) = S.lazily result
      (triple, ()) = S.lazily p2
  case triple of
    (-1,-1,-1) -> print "Part1 failed"
    (_,o,t) -> print $ o * t
  let pretty 0 = ' '
      pretty 1 = '.'
      pretty _ = '*'
  S.stdoutLn . S.mapped S.toList . chunksOf 25 . S.map pretty $ S.each pixels

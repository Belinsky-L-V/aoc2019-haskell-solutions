module Day8 (solve8) where

import Data.Maybe (fromMaybe)
import Streaming
import qualified Streaming.Prelude as S
import System.IO (Handle)

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

solve8 :: Handle -> IO String
solve8 handle = do
  let ints :: Stream (Of Int) IO ()
      ints = S.read . S.map pure . S.concat $ S.fromHandle handle
  result <- part2 $ S.store part1 ints
  let (pixels, p2) = S.lazily result
      (triple, ()) = S.lazily p2
  let part1out = case triple of
                   (-1,-1,-1) -> "Part1 failed"
                   (_,o,t) -> show $ o * t
  let pretty 0 = ' '
      pretty 1 = '.'
      pretty _ = '*'
  part2out <- fmap unlines . S.toList_ . S.mapped S.toList . chunksOf 25 . S.map pretty $ S.each pixels
  return $ part1out ++ "\n" ++ part2out

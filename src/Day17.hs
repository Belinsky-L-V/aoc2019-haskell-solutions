module Day17
  ( solve17
  ) where

import Data.Char (chr, ord)
import Data.List (intercalate, uncons)
import Data.List.HT (viewR)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import Intcode
import Streaming
import qualified Streaming.Prelude as S
import System.IO (Handle)

makeMap :: String -> Maybe (Int, Int, Map (Int, Int) Char)
makeMap output = do
  let outLines = lines output
      height = length outLines
  (firstLine, _) <- uncons outLines
  let width = length firstLine
      viewMap =
        Map.fromList
          [ ((fl, ft), c)
          | (ft, line) <- zip [0 ..] outLines
          , (fl, c) <- zip [0 ..] line
          ]
  return (width, height, viewMap)

intersections :: Map (Int, Int) Char -> [(Int, Int)]
intersections viewMap =
  let go acc (fl, ft) '#' =
        let maybeNeigh =
              (viewMap Map.!?) <$>
              [(pred fl, ft), (succ fl, ft), (fl, pred ft), (fl, succ ft)]
            neigh = length . filter (== '#') $ catMaybes maybeNeigh
         in if neigh == 4
              then (fl, ft) : acc
              else acc
      go acc _ _ = acc
   in Map.foldlWithKey' go [] viewMap

part1 :: Map (Int, Int) Char -> Int
part1 viewMap =
  let inters = intersections viewMap
   in foldr (\(fl,ft) acc -> acc + fl * ft) 0 inters

moveA = ["R","12","R","4","L","6","L","8","L","8"]
moveB = ["L","12","R","4","R","4"]
moveC = ["R","12","R","4","L","12"]
moveMain = ["B","C","C","A","A","B","B","C","C","A"]

formatInput :: [String] -> [Int]
formatInput = map ord . (++ "\n") . intercalate ","

part2 :: Monad m => Code -> ExceptT String m Int
part2 code = do
  let modCode = modifyIntcode code [(0,2)]
      input = concatMap formatInput [moveMain, moveA, moveB, moveC, ["n"]]
      outputStream = runProgram modCode (S.each input)
      (outputList, (outcome, _)) = S.lazily . runIdentity . S.toList $ outputStream
  (_,lastOutput) <- wrapMaybe "part2 failed" $ viewR outputList
  return lastOutput

solve17 :: Handle -> IO String
solve17 handle =
  fmap (either id id) . runExceptT $ do
    codeText <- lift $ Text.IO.hGetContents handle
    intCode <- readIntCode codeText
    outputNumList <- S.toList_ $ runProgram intCode (pure ())
    let outputCharList = map chr outputNumList
    (width, height, viewMap) <-
      wrapMaybe "failed to turn view into map" $ makeMap outputCharList
    let part1out = part1 viewMap
    part2out <- part2 intCode
    return . unlines . map show $ [part1out, part2out]

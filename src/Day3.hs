{-# LANGUAGE OverloadedStrings #-}

module Day3 (solve) where

import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Foldable (asum, foldl')
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sort)

type Parser = Parsec Void Text

data Direction = DUp | DDown | DLeft | DRight deriving (Show, Enum)

data Span = Span Direction Int deriving (Show)

parseDirection :: Parser Direction
parseDirection = asum $ zipWith (<$) [DUp ..] (single <$> "UDLR")

parseSpan :: Parser Span
parseSpan =  Span <$> parseDirection <*> L.decimal

parseSpanList :: Parser [Span]
parseSpanList = (:) <$> parseSpan <*> many (single ',' *> parseSpan)

parseBothWires = do
  wire1 <- parseSpanList
  single '\n'
  wire2 <- parseSpanList
  single '\n'
  eof
  return (wire1, wire2)

readInput :: IO ([Span], [Span])
readInput = do
  result <- parseMaybe parseBothWires <$> Text.IO.getContents
  case result of
    Nothing -> return ([],[])
    Just (first, second) -> return (first, second)

type VisitMap = Map (Int, Int) (Set Int)

tracePath :: [Span] -> VisitMap
tracePath spanlist =
  let visited = Map.singleton (0, 0) (Set.singleton 0)
      visit path (v,h,t) = case Map.lookup (v,h) path of
                             Nothing -> Map.insert (v,h) (Set.singleton t) path
                             Just times -> Map.insert (v,h) (Set.insert t times) path
      trace ((v,h,t), path) (Span dir len) =
        case dir of
          DUp -> ((v + len, h, t + len), foldl' visit path [(v + d, h, t + d) | d <- [1..len]])
          DDown -> ((v - len, h, t + len), foldl' visit path [(v - d, h, t + d) | d <- [1..len]])
          DRight -> ((v, h + len, t + len), foldl' visit path [(v, h + d, t + d) | d <- [1..len]])
          DLeft -> ((v, h - len, t + len), foldl' visit path [(v, h - d, t + d) | d <- [1..len]])
   in snd $ foldl' trace ((0,0,0), visited) spanlist

findClosest :: Set (Int, Int) -> Maybe (Int, (Int, Int))
findClosest intersections =
  let withDistance = Set.map (\(v, h) -> (abs v + abs h, (v, h))) intersections
   in if Set.size withDistance < 2 then Nothing else Just . Set.elemAt 1 $ withDistance

findShortest :: VisitMap -> VisitMap -> Set (Int, Int) -> Int
findShortest path1 path2 intersections =
  let common1 = Map.restrictKeys path1 intersections
      common2 = Map.restrictKeys path2 intersections
      combine k ts = Set.elemAt 0 ts + case Map.lookup k common2 of
                          Nothing -> 0
                          Just t2 -> Set.elemAt 0 t2
      distanceMap = Map.mapWithKey combine common1
      distances = sort $ Map.elems distanceMap
   in if length distances > 2 then distances !! 1
                              else 0

solve :: IO ()
solve = do
  (spans1, spans2) <- readInput
  let path1 = tracePath spans1
      path2 = tracePath spans2
      vset1 = Map.keysSet path1
      vset2 = Map.keysSet path2
      ints = Set.intersection vset1 vset2
  case findClosest ints of
    Nothing -> pure ()
    Just (d, (v, h)) -> print d
  print $ findShortest path1 path2 ints
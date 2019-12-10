module Day6 (solve6) where

import qualified Data.Graph.Inductive as FGL
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (foldl', span)
import System.IO (Handle, hGetContents)

part1 :: Gr String () -> Int
part1 = FGL.size . FGL.tc

part2 :: Gr String () -> FGL.NodeMap String -> Int
part2 gr nodeMap =
  let ((you,_),_) = FGL.mkNode nodeMap "YOU"
      ((san,_),_) = FGL.mkNode nodeMap "SAN"
      youOrb = head $ FGL.pre gr you
      sanOrb = head $ FGL.pre gr san
   in length (FGL.esp youOrb sanOrb (FGL.undir gr)) - 1

toGraph :: [(String, String)] -> (Maybe (Gr String ()), FGL.NodeMap String)
toGraph edgeLabels =
  let nodeLabels = uncurry (++) $ unzip edgeLabels
      (nlist, nodeMap) = FGL.mkNodes FGL.new nodeLabels
      edges = FGL.mkEdges nodeMap [(s,t,()) | (s,t) <- edgeLabels]
   in (FGL.mkGraph nlist <$> edges, nodeMap)

parseInput :: Handle -> IO [(String, String)]
parseInput handle = do
  direct <- lines <$> hGetContents handle
  return $ fmap tail . span (/= ')') <$> direct

solve6 :: Handle -> IO String
solve6 handle = do
  input <- parseInput handle
  case toGraph input of
    (Nothing, _) -> return "Failed to construct the graph from input :("
    (Just graph, nodeMap) -> return . unlines . map show $ [part1 graph, part2 graph nodeMap]

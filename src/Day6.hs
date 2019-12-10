module Day6 (solve6) where

import qualified Data.Graph.Inductive as FGL
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.List (span)
import System.IO (Handle, hGetContents)

part1 :: Gr String () -> FGL.NodeMap String -> Maybe Int
part1 gr nodeMap = do
  let (root,_) = FGL.mkNode_ nodeMap "COM"
      go graph node = do
        let (mcontext, forest) = FGL.match node graph
        (_, _, _, children) <- mcontext
        if null children
           then return (1, 0)
           else do
             (sizes, links) <- unzip <$> traverse (\(_,child) -> go forest child) children
             return (1 + sum sizes, sum sizes + sum links)
  snd <$> go gr root

part2 :: Gr String () -> FGL.NodeMap String -> Int
part2 gr nodeMap =
  let (you,_) = FGL.mkNode_ nodeMap "YOU"
      (san,_) = FGL.mkNode_ nodeMap "SAN"
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
    (Nothing, _) -> return "Failed to construct the graph from input :(\n"
    (Just graph, nodeMap) -> do
      let part1out = maybe "Part 1 failed :(\n" show (part1 graph nodeMap)
          part2out = show $ part2 graph nodeMap
      return $ unlines [part1out, part2out]

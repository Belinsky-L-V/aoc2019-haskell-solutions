{-# LANGUAGE RecordWildCards #-}

module Day15
  ( solve15
  ) where

import Control.Foldl (Fold(..), fold)
import qualified Control.Foldl as Foldl
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Graph.Inductive
import Data.List (find, uncons)
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import qualified Data.Text.IO as Text.IO
import Intcode
import Streaming
import qualified Streaming.Prelude as S
import System.IO (Handle)

data Direction
  = DirNorth
  | DirSouth
  | DirWest
  | DirEast
  deriving (Eq, Show)

type Position = Int

type MazeGraph = Gr () Direction

type MazeCrawler
   = (Position, Seq Position, MazeGraph, Int, IntCodeVM, Maybe Position)

data MazeTile
  = TileWall
  | TileEmpty
  | TileOx
  deriving (Eq, Show)

dirToInt :: Direction -> Int
dirToInt DirNorth = 1
dirToInt DirSouth = 2
dirToInt DirWest = 3
dirToInt DirEast = 4

intToTile :: Int -> Maybe MazeTile
intToTile 0 = Just TileWall
intToTile 1 = Just TileEmpty
intToTile 2 = Just TileOx
intToTile _ = Nothing

invertDir :: Direction -> Direction
invertDir DirNorth = DirSouth
invertDir DirSouth = DirNorth
invertDir DirWest = DirEast
invertDir DirEast = DirWest

addNeighb :: [(Direction, Int)] -> Int -> ([(Direction, Int)], Int)
addNeighb edgeList nextNodeNum =
  let add (es, nn) checking =
        if elem checking $ fst <$> edgeList
          then (es, nn)
          else ((checking, nn) : es, succ nn)
   in fold
        (Fold add ([], nextNodeNum) id)
        [DirNorth, DirSouth, DirWest, DirEast]

pathToEdges :: Monad m => Path -> MazeGraph -> ExceptT String m [Direction]
pathToEdges path maze = go path
  where
    go (f:t:rest) = do
      dir <-
        wrapMaybe "Could not find a path to node." $
        find
          (\d -> hasLEdge maze (f, t, d))
          [DirNorth, DirSouth, DirWest, DirEast]
      tailPath <- go (t : rest)
      return $ dir : tailPath
    go _ = pure []

unconsSeq :: Seq a -> Maybe (a, Seq a)
unconsSeq Empty = Nothing
unconsSeq (h :<| rest) = Just (h, rest)

travelToNext :: ExceptT String (State MazeCrawler) (MazeTile, Position)
travelToNext = do
  (pos, toVisit, maze, nextNodeNum, vm@IntCodeVM {..}, ox) <- lift get
  (next, rest) <-
    wrapMaybe "Could not find oxygen in the maze." $ unconsSeq toVisit
  let (mcontext, restOfMaze) = match next maze
  (inList, _, _, outList) <-
    wrapMaybe "Could not find the node to visit in the graph." mcontext
  case find ((== pos) . snd) inList of
    Just (dir, _) -> do
      finalStatus <- moveAndRead [dir]
      case finalStatus of
        TileOx -> return (finalStatus, next)
        TileWall -> return (finalStatus, pos)
        TileEmpty -> return (finalStatus, next)
    Nothing -> do
      let path = esp pos next maze
      edgePath <- pathToEdges path maze
      finalStatus <- moveAndRead edgePath
      let finalPos = last . init $ path
      case finalStatus of
        TileOx -> return (finalStatus, next)
        TileWall -> return (finalStatus, finalPos)
        TileEmpty -> return (finalStatus, next)
  where
    moveAndRead :: [Direction] -> ExceptT String (State MazeCrawler) MazeTile
    moveAndRead [] = throwE "Tried to travel on a zero length path."
    moveAndRead dirs = do
      (pos, toVisit, maze, nextNodeNum, vm, ox) <- lift get
      let input = S.each (dirToInt <$> dirs)
          outputStream = runIntCodeVM vm {inputBuf = input}
      (outputList, (state, newVm@IntCodeVM {..})) <-
        S.lazily <$> S.toList outputStream
      wrapMaybe "Failed to return status." $ uncons outputList
      finalStatus <- wrapMaybe "Invalid status." $ intToTile (last outputList)
      case state of
        ReadFromEmpty _ -> do
          let rewVM = newVm {instrPointer = instrPointer - 1}
          lift $ put (pos, toVisit, maze, nextNodeNum, rewVM, ox)
          return finalStatus
        Success -> throwE "Intcode halted"
        _ -> throwE "Intcode failed"

mapMaze :: ExceptT String (State MazeCrawler) ()
mapMaze = do
  (pos, toVisit, maze, nextNodeNum, vm, ox) <- lift get
  case unconsSeq toVisit of
    Nothing -> return ()
    Just (next, rest) -> do
      let (mcontext, restOfMaze) = match next maze
      (_, _, _, outList) <-
        wrapMaybe "Could not find the node to visit in the graph" mcontext
      (tileAtNext, finalNext) <- travelToNext
      (_, _, _, _, newVm, _) <- lift get
      if tileAtNext == TileWall
        then do
          lift $ put (finalNext, rest, maze, nextNodeNum, newVm, ox)
          mapMaze
        else do
          let (addEdges, newNextNodeNum) = addNeighb outList nextNodeNum
              newOutAdj = outList ++ addEdges
              newNodes = snd <$> addEdges
              newToVisit = Seq.fromList newNodes <|> rest
              mazeWithNodes =
                fold
                  (Fold (\acc node -> insNode (node, ()) acc) restOfMaze id)
                  newNodes
              newInAdj = [(invertDir dir, node) | (dir, node) <- newOutAdj]
              newMaze = (newInAdj, next, (), newOutAdj) & mazeWithNodes
              newOx =
                case tileAtNext of
                  TileOx -> Just next
                  _ -> ox
          lift $ put (next, newToVisit, newMaze, newNextNodeNum, newVm, newOx)
          mapMaze

setupMazeCrawler :: IntCodeVM -> MazeCrawler
setupMazeCrawler vm =
  let maze :: MazeGraph
      maze =
        mkGraph
          [(n, ()) | n <- [0 .. 4]]
          [ (0, 1, DirNorth)
          , (0, 2, DirSouth)
          , (0, 3, DirWest)
          , (0, 4, DirEast)
          , (1, 0, DirSouth)
          , (2, 0, DirNorth)
          , (3, 0, DirEast)
          , (4, 0, DirWest)
          ]
      pos = 0
      toVisit = Seq.fromList [1 .. 4]
      nextNodeNum = 5
   in (pos, toVisit, maze, nextNodeNum, vm, Nothing)

solve' :: Monad m => Code -> ExceptT String m (Int, Int)
solve' code = do
  let vm = initVM code (pure ())
      initState = setupMazeCrawler vm
      (result, finalState) = flip runState initState . runExceptT $ mapMaze
  except result
  let (_, _, maze, _, _, maybeOxAt) = finalState
  oxAt <- wrapMaybe "oxygen not found" maybeOxAt
  let originToOxSteps = length (esp 0 oxAt maze) - 1
      shortestPathsFromOx = bft oxAt maze
      longestShortestPath = map length shortestPathsFromOx
  toFillUp <-
    wrapMaybe "Failed to construct shortest paths" $
    fold Foldl.maximum longestShortestPath
  return (originToOxSteps, toFillUp - 2)

solve15 :: Handle -> IO String
solve15 handle =
  fmap (either id id) . runExceptT $ do
    codeText <- lift $ Text.IO.hGetContents handle
    intCode <- readIntCode codeText
    (part1out, part2out) <- solve' intCode
    return . unlines . map show $ [part1out, part2out]

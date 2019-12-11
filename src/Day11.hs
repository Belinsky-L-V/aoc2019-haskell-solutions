module Day11
  ( solve11
  ) where

import Control.Foldl (Fold(..))
import qualified Control.Foldl as Foldl
import Control.Monad.Trans.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import Intcode
import Streaming
import qualified Streaming.Prelude as S
import System.IO (Handle)

data Colour
  = Black
  | White
  deriving (Eq, Enum, Read)

instance Show Colour where
  show Black = " "
  show White = "*"

data Direction
  = DirUp
  | DirRight
  | DirDown
  | DirLeft
  deriving (Eq, Show, Enum, Bounded, Read)

type Position = (Int, Int)

type Hull = Map Position Colour

type Turn = Direction -> Direction

type RobotState = State (Position, Direction, Hull, IntCodeVM)

advance :: Position -> Direction -> Position
advance (x, y) DirUp = (succ x, y)
advance (x, y) DirRight = (x, succ y)
advance (x, y) DirDown = (pred x, y)
advance (x, y) DirLeft = (x, pred y)

next :: (Enum a, Bounded a) => a -> a
next = turn 1

prev :: (Enum a, Bounded a) => a -> a
prev = turn (-1)

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
  where
    add mod x y = (x + y + mod) `rem` mod

paintAndMove :: (Colour, Turn) -> RobotState ()
paintAndMove (colour, turn) = do
  (pos, dir, hull, vm) <- get
  let newHull = Map.insert pos colour hull
      newDir = turn dir
      newPos = advance pos newDir
  put (newPos, newDir, newHull, vm)
  return ()

checkColour :: Position -> Hull -> Colour
checkColour = Map.findWithDefault Black

parseOutput :: Monad m => [Int] -> ExceptT String m (Colour, Turn)
parseOutput [c, t]
  | c `notElem` [0, 1] = throwE "Failed colour parse"
  | t `notElem` [0, 1] = throwE "Failed turn parse"
  | otherwise = return (toEnum c, parseTurn t)
  where
    parseTurn 0 = prev
    parseTurn _ = next
parseOutput _ = throwE "Malformed output"

feedInputs :: ExceptT String RobotState [(Colour, Turn)]
feedInputs = do
  (pos, dir, hull, vm@IntCodeVM {inputBuf = inputBuf}) <- lift get
  let colour = checkColour pos hull
      outputStream =
        runIntCodeVM vm {inputBuf = inputBuf >> S.yield (fromEnum colour)}
  let toParse =
        hoist (return . runIdentity) . S.mapped S.toList . chunksOf 2 $
        outputStream
  parsedOutput <- S.toList_ . S.mapM parseOutput $ toParse
  let st@(state, newVM) = runIdentity . S.effects $ outputStream
  if null parsedOutput
    then case state of
           Success -> do
             lift $ put (pos, dir, hull, newVM)
             return parsedOutput
           ReadFromEmpty _ -> do
             reset <- wrapMaybe "couldn't rollback" $ rollbackFailure st
             lift $
               put
                 (pos, dir, hull, reset {inputBuf = S.yield (fromEnum colour)})
             feedInputs
           err -> throwE $ show err
    else case state of
           Success -> do
             lift $ put (pos, dir, hull, newVM)
             return parsedOutput
           ReadFromEmpty _ -> do
             reset <- wrapMaybe "couldn't rollback" $ rollbackFailure st
             lift $ put (pos, dir, hull, reset)
             return parsedOutput
           err -> throwE $ show err

runRobot :: ExceptT String RobotState ()
runRobot = do
  outputs <- feedInputs
  if null outputs
    then return ()
    else do
      lift $ mapM_ paintAndMove outputs
      runRobot

setUpAndRun code colour =
  let vm = initVM code (pure ())
      hull = Map.singleton (0, 0) colour
      pos = (0, 0)
      dir = DirUp
      robot = (pos, dir, hull, vm)
      (err, finishedRobot) = runIdentity $ runStateT (runExceptT runRobot) robot
      (_, _, finalHull, _) = finishedRobot
   in except $ second (const finalHull) err

part1 :: Monad m => Code -> ExceptT String m Int
part1 code = do
  finalHull <- setUpAndRun code Black
  let paintedSquares = Map.size finalHull
      hullWhite = Map.singleton (0, 0) White
  return paintedSquares

part2 :: Monad m => Code -> ExceptT String m String
part2 code = do
  finalHull <- setUpAndRun code White
  let items = Map.toList finalHull
      (xs, ys) = unzip . fst $ unzip items
      rangeStep Nothing x = Just (x, x)
      rangeStep (Just (small, big)) x = Just (min small x, max big x)
  (xMin, xMax) <-
    wrapMaybe "Part 2: Failed to get range of x" $
    Foldl.fold (Fold rangeStep Nothing id) xs
  (yMin, yMax) <-
    wrapMaybe "Part 2: Failed to get range of y" $
    Foldl.fold (Fold rangeStep Nothing id) ys
  let hullImage =
        [ [Map.findWithDefault Black (x, y) finalHull | x <- [xMin .. xMax]]
        | y <- [yMin .. yMax]
        ]
  return . unlines . map (concatMap show) $ hullImage

solve11 :: Handle -> IO String
solve11 handle =
  fmap (either id id) . runExceptT $ do
    codeText <- lift $ Text.IO.hGetContents handle
    intCode <- readIntCode codeText
    part1out <- part1 intCode
    part2out <- part2 intCode
    return (show part1out ++ "\n" ++ part2out)

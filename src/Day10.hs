module Day10 (solve10, Rep, rep) where

import Control.Foldl (Fold(..))
import qualified Control.Foldl as Foldl
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (Handle, hGetContents)

type Position = (Int, Int)
type AngleMap = Map Rep [Position]

newtype Rep = Rep (Int, Int) deriving (Show, Eq)

instance Ord Rep where
  Rep (x,y) <= Rep (ox,oy)
    | x == 0 && y == 0 = True
    | x == 0 && y < 0 = (ox,oy) /= (0,0)
    | x == 0 && y > 0 = ox < 0 || ox == 0 && oy > 0
    | ox == 0 && oy == 0 = (x,y) == (0,0)
    | ox == 0 && oy < 0 = x == 0 && y <= 0
    | ox == 0 && oy > 0 = x >= 0
    | x > 0 && ox > 0 = y % x <= oy % ox
    | x > 0 && ox < 0 = True
    | x < 0 && ox > 0 = False
    | x < 0 && ox < 0 = y % x <= oy % ox

parseMap :: String -> Set Position
parseMap input =
  let mapLines = lines input
   in Set.fromList
        [ (x, y)
        | (y, line) <- zip [0 ..] mapLines
        , (x, c) <- zip [0 ..] line
        , c == '#'
        ]

rep :: Position -> Rep
rep (0,0) = Rep (0,0)
rep (x,y) =
  let d = gcd x y
      rx = quot x d
      ry = quot y d
   in Rep (rx, ry)

findAngles :: Set Position -> Position -> AngleMap
findAngles asteroids (x, y) =
  let relative (ox, oy) = (ox - x, oy - y)
      buildRel :: AngleMap -> Position -> AngleMap
      buildRel acc o@(ox, oy) =
        let key = rep (relative o)
            alterF Nothing = Just [o]
            alterF (Just as) = Just $ o : as
         in if (x, y) == o
              then acc
              else Map.alter alterF key acc
   in Foldl.fold (Fold buildRel Map.empty id) asteroids

anglesForAll :: Set Position -> Map Position AngleMap
anglesForAll asteroids = Map.fromSet (findAngles asteroids) asteroids

part1 :: Set Position -> (Position, Int)
part1 asteroids =
  let angles = anglesForAll asteroids
      canSee = Map.map Map.size angles
      maxSee old@(coord,sees) newc newsees =
        if newsees > sees then (newc, newsees)
                          else old
   in Map.foldlWithKey' maxSee ((0,0),0) canSee

part2 :: Set Position -> Position -> Position
part2 asteroids station =
  let stationAngles = findAngles asteroids station
      laser (destroyed, acc, sol) rep (pos:rest) =
        let newSol =
              if succ destroyed == 200
                then pos
                else sol
            newAcc = Map.insert rep rest acc
         in (succ destroyed, newAcc, newSol)
      laser (destroyed, acc, sol) rep [] =
        let newAcc = Map.delete rep acc
         in (destroyed, newAcc, sol)
      oneRound (des, acc, sol) =
        Map.foldlWithKey' laser (des, Map.empty, sol) acc
      (_, _, solution) =
        until (\(d, _, _) -> d >= 200) oneRound (0, stationAngles, (0, 0))
   in solution

solve10 :: Handle -> IO String
solve10 handle =
  fmap (either id id) . runExceptT $ do
    input <- lift $ hGetContents handle
    let asteroids = parseMap input
    let (stationAt, part1out) = part1 asteroids
    let (x, y) = part2 asteroids stationAt
    return . unlines . map show $ [part1out, 100 * x + y]

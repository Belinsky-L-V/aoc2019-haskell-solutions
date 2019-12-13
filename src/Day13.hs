module Day13 (solve13) where

import Control.Foldl (Fold(..), fold)
import Control.Monad.Trans.State
import Data.Maybe (fromMaybe)
import qualified Data.Text.IO as Text.IO
import Intcode
import Streaming
import qualified Streaming.Prelude as S
import System.IO (Handle)

every :: Int -> [a] -> [a]
every n xs =
  case drop (n - 1) xs of
    (y:ys) -> y : every n ys
    [] -> []

groupsOf :: Int -> [a] -> [[a]]
groupsOf n xs =
  let (group, rest) = splitAt n xs
   in case rest of
        [] -> [group]
        _ -> group : groupsOf n rest

move :: Ball -> Paddle -> Int
move (ballx, _) (padx, _) =
  case compare ballx padx of
    LT -> -1
    EQ -> 0
    GT -> 1

type Score = Int
type Move = Int
type Ball = (Int, Int)
type Paddle = (Int, Int)

examineOutput :: [Int] -> (Maybe Score, Maybe Ball, Maybe Paddle)
examineOutput out =
  let triples = groupsOf 3 out
      checkTriple acc@(score, ball, paddle) [x, y, t] =
        if (x, y) == (-1, 0)
          then (Just t, ball, paddle)
          else case t of
                 3 -> (score, ball, Just (x, y))
                 4 -> (score, Just (x, y), paddle)
                 _ -> acc
      checkTriple acc _ = acc
   in fold (Fold checkTriple (Nothing, Nothing, Nothing) id) triples

feedback :: [Int] -> State (Score, Ball, Paddle) InputStream
feedback out = do
  (score, ball, paddle) <- get
  let (mayScore, mayBall, mayPaddle) = examineOutput out
      newScore = fromMaybe score mayScore
      newBall = fromMaybe ball mayBall
      newPaddle = fromMaybe paddle mayPaddle
      makingMove = move newBall newPaddle
  put (newScore, newBall, newPaddle)
  return (S.yield makingMove)

part2 :: Code -> Score
part2 code =
  let newCode = modifyIntcode code [(0, 2)]
      vm = initVM newCode (pure ())
      game = runWithFeedbackM feedback vm
      ((out, vmRes), (score, _, _)) = runState game (-1, (0, 0), (0, 0))
      (mayScore, _, _) = examineOutput out
   in fromMaybe score mayScore

part1 :: Code -> Int
part1 code = length . filter (== 2) . every 3 $ outputList
  where
    outputList = runIdentity . S.toList_ $ runProgram code (pure ())

solve13 :: Handle -> IO String
solve13 handle =
  fmap (either id id) . runExceptT $ do
    codeText <- lift $ Text.IO.hGetContents handle
    intCode <- readIntCode codeText
    let part1out = part1 intCode
    let part2out = part2 intCode
    return . unlines . map show $ [part1out, part2out]

module Day13 where

import qualified Data.Text.IO as Text.IO
import Intcode
import Streaming
import qualified Streaming.Prelude as S
import System.IO (Handle)

part1 :: Code -> Int
part1 code = length . filter (== 2) . every 3 $ outputList
  where
    outputList = runIdentity . S.toList_ $ runProgram code (pure ())

every :: Int -> [a] -> [a]
every n xs =
  case drop (n - 1) xs of
    (y:ys) -> y : every n ys
    [] -> []

solve13 :: Handle -> IO String
solve13 handle =
  fmap (either id id) . runExceptT $ do
    codeText <- lift $ Text.IO.hGetContents handle
    intCode <- readIntCode codeText
    let part1out = show $ part1 intCode
    return $ part1out ++ "\n"

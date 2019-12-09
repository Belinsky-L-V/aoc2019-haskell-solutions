module Day7 where

import Control.Foldl (Fold(..))
import qualified Control.Foldl as Foldl
import Data.List (permutations)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import Intcode
import Streaming (Of, Stream)
import qualified Streaming as S
import qualified Streaming.Prelude as SP

runChain :: Code -> [Int] -> [Int]
runChain code perm =
  let inputs :: [Stream (Of Int) Identity ()]
      inputs = SP.yield <$> perm
      amp = runProgram code
      step acc next = () <$ amp (next >> acc)
      finalStream = Foldl.fold (Fold step (SP.yield 0) id) inputs
   in runIdentity $ SP.toList_ finalStream

findBestInput :: Code -> Maybe Int
findBestInput code =
  let step (Just m) [x] = Just $ max m x
      step Nothing [x] = Just x
      step _ _ = Nothing
      getMax = Foldl.fold (Fold step Nothing id)
   in getMax $ runChain code <$> permutations [0..4]

solve :: IO ()
solve = do
  codeText <- Text.IO.getContents
  codeParse <- runExceptT (readIntCode codeText)
  case codeParse of
    Left err -> putStrLn err
    Right intCode -> print $ findBestInput intCode

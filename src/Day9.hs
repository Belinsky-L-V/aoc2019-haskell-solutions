module Day9 (solve9) where

import Intcode
import Control.Foldl (Fold(..))
import qualified Control.Foldl as Foldl
import qualified Data.IntMap as IntMap
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import qualified Streaming.Prelude as S
import System.IO (Handle)

solve9 :: Handle -> IO String
solve9 handle = fmap (either id id) . runExceptT $ do
  codeText <- lift $ Text.IO.hGetContents handle
  intCode <- readIntCode codeText
  part1out <- case S.lazily . runIdentity . S.toList $ runProgram intCode (S.yield 1) of
                (out, (Success,_)) -> return $ unwords . map show $ out
                (out, (err, _)) -> throwE $ "Part 1 failed.\n" ++ show err
  part2out <- case S.lazily . runIdentity . S.toList $ runProgram intCode (S.yield 2) of
                (out, (Success,_)) -> return $ unwords . map show $ out
                (out, (err, _)) -> throwE $ "Part 2 failed.\n" ++ show err
  return $ unlines [part1out, part2out]

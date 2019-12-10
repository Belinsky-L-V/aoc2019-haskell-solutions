module Day5 (solve5) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import Intcode
import Streaming
import qualified Streaming.Prelude as S
import System.IO (Handle)

solve' :: Code -> Int -> String
solve' intCode x = unwords . map show $ runIdentity $ S.toList_ $ runProgram intCode (S.yield x)

solve5 :: Handle -> IO String
solve5 handle = fmap (either id id) . runExceptT $ do
  codeText <- lift $ Text.IO.hGetContents handle
  intCode <- readIntCode codeText
  return $ unlines [solve' intCode 1, solve' intCode 5]

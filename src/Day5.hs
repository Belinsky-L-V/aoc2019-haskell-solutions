module Day5 where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import Intcode
import qualified Streaming as S
import qualified Streaming.Prelude as SP
import System.IO (Handle)

solve' :: Code -> Int -> IO ()
solve' intCode x =
  (>> pure ()) . runExceptT $ do
    (state, vm) <-
      S.liftIO $
      SP.print $ S.hoist (pure . runIdentity) $ runProgram intCode (SP.each [x])
    case state of
      Left err -> S.liftIO $ putStrLn err
      Right () -> pure ()

solve :: Handle -> IO ()
solve handle = do
  codeText <- Text.IO.hGetContents handle
  codeParse <- runExceptT (readIntCode codeText)
  case codeParse of
    Left err -> putStrLn err
    Right intCode -> solve' intCode 1 >> solve' intCode 5

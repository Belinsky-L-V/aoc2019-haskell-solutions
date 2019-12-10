module Day7 where

import Control.Foldl (Fold(..))
import qualified Control.Foldl as Foldl
import qualified Data.IntMap as IntMap
import Data.List (permutations)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import Intcode
import Streaming
import qualified Streaming.Prelude as S
import System.IO (Handle)

clobber :: InputStream -> OutputStream
clobber stream =
  let code = IntMap.empty
   in (Left "Initial placeholder", IntCodeVM Halt code 0 (pure ())) <$ stream

ampChain :: Code -> [Int] -> OutputStream -> OutputStream
ampChain code phases initial =
  let inputs :: [Stream (Of Int) Identity ()]
      inputs = S.yield <$> phases
      amp = runProgram code
      step acc next = amp (next >> (() <$ acc))
   in Foldl.fold (Fold step initial id) inputs

runChain :: Code -> [Int] -> [Int]
runChain code phases =
  let permChain = ampChain code phases . clobber $ S.yield 0
   in runIdentity . S.toList_ $ permChain

-- shlemiel, the amp feedbacker, yikes
cycleChain :: (OutputStream -> OutputStream) -> OutputStream -> Int
cycleChain chain stream =
  case S.lazily . runIdentity . S.toList $ chain stream of
    (output, (Right (), _)) -> last output
    (output, _) -> cycleChain chain (clobber $ S.each (0:output))

fixChain :: Code -> [Int] -> Int
fixChain code phases =
  let chain = ampChain code phases
   in cycleChain chain (clobber $ S.yield 0)

getMax :: [[Int]] -> Maybe Int
getMax =
  let step (Just m) [x] = Just $ max m x
      step Nothing [x] = Just x
      step _ _ = Nothing
   in Foldl.fold (Fold step Nothing id)

part1 :: Code -> Maybe Int
part1 code = getMax $ runChain code <$> permutations [0..4]

part2 :: Code -> Maybe Int
part2 code = Foldl.fold Foldl.maximum $ fixChain code <$> permutations [5..9]

solve :: Handle -> IO ()
solve handle = do
  codeText <- Text.IO.hGetContents handle
  codeParse <- runExceptT (readIntCode codeText)
  case codeParse of
    Left err -> putStrLn err
    Right intCode -> print (part1 intCode) >> print (part2 intCode)

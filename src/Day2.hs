module Day2 (solve2) where

import qualified Data.IntMap as IntMap
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import qualified Data.Text.Read as Text.Read
import Intcode
import qualified Streaming.Prelude as S
import System.IO (Handle)

solve2 :: Handle -> IO String
solve2 handle = fmap (either id id) . runExceptT $ do
  let changes = [(1, 12), (2, 2)]
  codeText <- lift $ Text.IO.hGetContents handle
  intCode <- readIntCode codeText
  let (_, vm) = runIdentity . S.effects $ runProgram (modifyIntcode intCode changes) (pure ())
      readAtZero vmr = wrapMaybe "Machine doesn't have adress 0 in memory" $ IntMap.lookup 0 (code vmr)
  part1 <- readAtZero vm
  let part1out = [show part1]
  let results =
        [ (noun, verb)
        | noun <- [1 .. 99]
        , verb <- [1 .. 99]
        , let (_, vmr) = runIdentity . S.effects $ runProgram (modifyIntcode intCode [(1, noun),(2, verb)]) (pure ())
        , Just 19690720 == IntMap.lookup 0 (code vmr)
        ]
  case results of
    [] -> throwE "Failed part 2"
    ((noun, verb):_) -> return $ unlines (part1out ++ [show $ 100 * noun + verb])

module Day7 (solve7) where

import Control.Foldl (Fold(..))
import qualified Control.Foldl as Foldl
import Control.Monad.Trans.State
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
  let code = IntMap.empty in (Success, dummyVM) <$ stream

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

listOut = S.lazily . runIdentity . S.toList

daisyChain :: IntCodeVM -> ExceptT Outcome (State OutputStream) IntCodeVM
daisyChain vm@IntCodeVM {inputBuf = inputBuf} = do
  upChainOut <- lift get
  let outputStream = runIntCodeVM vm {inputBuf = inputBuf >> (() <$ upChainOut)}
  case runIdentity $ S.effects outputStream of
    (Success, newVM) -> do
      lift . put $ outputStream
      return newVM
    st@(ReadFromEmpty _, newVM) -> do
      reset <- wrapMaybe (ReadFromEmpty "") $ rollbackFailure st
      lift . put $ outputStream
      return reset
    (err, _) -> throwE err

cycleChain :: Monad m => [IntCodeVM] -> InputStream -> ExceptT String m Int
cycleChain vms stream = do
  let next = runIdentity . flip runStateT (clobber stream) . runExceptT $ mapM daisyChain vms
  case next of
    (Right newVMs, finalOutput) -> do
      let finalState = runIdentity . S.effects $ finalOutput
      case finalState of
        (Success, _) -> do
          let outputList = runIdentity $ S.toList_ finalOutput
          if null outputList then throwE "Amp chain didn't have an output."
                             else return (last outputList)
        (ReadFromEmpty _, _) -> cycleChain newVMs (() <$ finalOutput)
        (err, _) -> throwE $ "Chain failed.\n" ++ show err

fixChain :: Monad m => Code -> [Int] -> ExceptT String m Int
fixChain code phases = do
  let chain = initVM code . S.yield <$> phases
  cycleChain chain (S.yield 0)

part2 :: Monad m => Code -> ExceptT String m Int
part2 code = do
  maxThrusts <- mapM (fixChain code) (permutations [5..9])
  wrapMaybe "Part 2: failed to fold thrust outputs" $ Foldl.fold Foldl.maximum maxThrusts

getMax :: [[Int]] -> Maybe Int
getMax =
  let step (Just m) [x] = Just $ max m x
      step Nothing [x] = Just x
      step _ _ = Nothing
   in Foldl.fold (Fold step Nothing id)

part1 :: Code -> Maybe Int
part1 code = getMax $ runChain code <$> permutations [0..4]

solve7 :: Handle -> IO String
solve7 handle = fmap (either id id) . runExceptT $ do
  codeText <- lift $ Text.IO.hGetContents handle
  intCode <- readIntCode codeText
  part1out <- wrapMaybe "Part 1 failed\n" $ part1 intCode
  part2out <- part2 intCode
  return . unlines . map show $ [part1out , part2out]

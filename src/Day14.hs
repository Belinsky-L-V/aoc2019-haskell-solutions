{-# LANGUAGE OverloadedStrings #-}

module Day14 (solve14) where

import Control.Foldl (Fold(..), fold)
import qualified Control.Foldl as Foldl
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Bifunctor
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Void
import Numeric.Search (divForever, largest, positiveExponential, searchM)
import System.IO (Handle)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text
type Chemical = String
type Amount = Int
type Product = (Chemical, Amount)
type Inputs  = [Product]
type Leftovers  = Map Chemical Amount
type Reaction = (Inputs, Product)

spaceConsumer = Lexer.space space1 empty empty
lexeme  = Lexer.lexeme spaceConsumer

parseChem :: Parser Chemical
parseChem = lexeme (some letterChar)

parseAmount :: Parser Amount
parseAmount = lexeme Lexer.decimal

parseProduct :: Parser Product
parseProduct = flip (,) <$> parseAmount <*> parseChem

parseReaction :: Parser Reaction
parseReaction = (,) <$> (parseProduct `sepBy` string ", ") <*> (string "=> " *> parseProduct)

parseAll :: Parser [Reaction]
parseAll = many parseReaction <* eof

parseInput :: Monad m => Text -> ExceptT String m [Reaction]
parseInput input = except . first errorBundlePretty $ parse parseAll "" input

topSortDAG :: Map Chemical Inputs -> [Chemical]
topSortDAG dependsOn =
  let visit :: Chemical -> State (Set Chemical, [Chemical]) ()
      visit chem = do
        (toVisit, sorted) <- get
        when (Set.member chem toVisit) $ do
          mapM_ (visit . fst) . fromMaybe [] $ Map.lookup chem dependsOn
          (toVisit, sorted) <- get
          let newToVisit = Set.delete chem toVisit
              newSorted = chem : sorted
          put (newToVisit, newSorted)
      go :: State (Set Chemical, [Chemical]) ()
      go = do
        (toVisit, sorted) <- get
        unless (Set.null toVisit) $ do
          visit (Set.elemAt 0 toVisit)
          go
   in reverse . snd . snd $ runState go (Map.keysSet dependsOn, [])

wrapMaybe str = maybeToExceptT str . MaybeT . pure

costOfReaction ::
     Map Chemical (Amount, Inputs)
  -> Chemical
  -> Amount
  -> ExceptT String (State Leftovers) Amount
costOfReaction reactions "ORE" n = return n
costOfReaction reactions chem n = do
  (amount, inputs) <-
    wrapMaybe "Chemical not in map." $ Map.lookup chem reactions
  leftovers <- lift get
  let inLeftovers = fromMaybe 0 $ Map.lookup chem leftovers
  let toProduce = n - inLeftovers
  if inLeftovers >= n
    then do
      lift . put $ Map.insert chem (inLeftovers - n) leftovers
      return 0
    else do
      let multiple =
            if amount >= toProduce
              then 1
              else quot toProduce amount + fromEnum (rem toProduce amount > 0)
      let costOfInput (ingred, ingredAmount) =
            costOfReaction reactions ingred (ingredAmount * multiple)
      oreInInputs <- mapM costOfInput inputs
      leftovers <- lift get
      lift . put $ Map.insert chem (amount * multiple - toProduce) leftovers
      return $ sum oreInInputs

part1 :: Monad m => Map Chemical (Amount, Inputs) -> ExceptT String m Int
part1 reactions = do
  let result = costOfReaction reactions "FUEL" 1
  except . fst . flip runState Map.empty . runExceptT $ result

part2 :: Monad m => Map Chemical (Amount, Inputs) -> ExceptT String m Int
part2 reactions = do
  let rewrap = except . fst . flip runState Map.empty . runExceptT
      initialised n = rewrap $ costOfReaction reactions "FUEL" n
      searchWith n = (<= 1000000000000) <$> initialised n
  range <- searchM positiveExponential divForever searchWith
  wrapMaybe "Binary search failed" $ largest True range

reactionMap :: [Reaction] -> Map Chemical [(Amount, Inputs)]
reactionMap reactionList =
  let addReaction acc (inputs, (chem, amount)) =
        let adjustMap Nothing = Just [(amount, inputs)]
            adjustMap (Just rs) = Just $ (amount, inputs):rs
         in Map.alter adjustMap chem acc
   in fold (Fold addReaction Map.empty id) reactionList

simpleReactionMap ::
     Monad m
  => Map Chemical [(Amount, Inputs)]
  -> ExceptT String m (Map Chemical (Amount, Inputs))
simpleReactionMap reactions =
  if any (/= 1) $ Map.map length reactions
    then throwE "A chemical doesn't have exactly 1 way to produce it"
    else return $ Map.map head reactions

sortedReactionMap ::
     Monad m
  => Map Chemical (Amount, Inputs)
  -> ExceptT String m (Map Chemical (Amount, Inputs))
sortedReactionMap reactions = do
  let topSortList = topSortDAG $ Map.map snd reactions
      rankMap = Map.fromList $ ("ORE", -1) : zip topSortList [0 ..]
      orderChems :: Chemical -> Chemical -> Ordering
      orderChems a b =
        let ranka = rankMap Map.! a
            rankb = rankMap Map.! b
         in compare ranka rankb
      allComponentsRanked =
        not . or $
        Map.map
          (elem Nothing . map ((`Map.lookup` rankMap) . fst) . snd)
          reactions
      allResultsRanked =
        notElem Nothing . map (`Map.lookup` rankMap) $ Map.keys reactions
      sortInputs (amount, inputs) =
        (amount, sortBy (\(a, _) (b, _) -> orderChems a b) inputs)
  if allResultsRanked && allComponentsRanked
    then return $ Map.map sortInputs reactions
    else throwE "Could not topologically sort chemicals."

solve14 :: Handle -> IO String
solve14 handle =
  fmap (either id id) . runExceptT $ do
    input <- lift $ Text.IO.hGetContents handle
    parsed <- parseInput input
    reactions <- sortedReactionMap =<< simpleReactionMap (reactionMap parsed)
    part1out <- part1 reactions
    part2out <- part2 reactions
    return . unlines . map show $ [part1out, part2out]

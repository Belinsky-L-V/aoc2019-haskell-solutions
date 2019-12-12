{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Day12 where

import Control.DeepSeq
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.List (transpose)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Void
import System.IO (Handle)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Parser = Parsec Void Text

spaceConsumer = Lexer.space space1 empty empty
lexeme  = Lexer.lexeme spaceConsumer

parseAxis :: Parser Char
parseAxis = anySingle

parseVal :: Parser Int
parseVal = Lexer.signed spaceConsumer Lexer.decimal

parseCoord :: Parser (Char, Int)
parseCoord = (,) <$> parseAxis <*> (char '=' *> parseVal)

parseMoon :: Parser [(Char, Int)]
parseMoon = char '<' *> (parseCoord `sepBy` lexeme (char ',')) <* lexeme (char '>')

parseAll :: Parser [[(Char, Int)]]
parseAll = many parseMoon <* eof

type CoordsOfMoon = [Int]
type PositionOnAxis = [Int]
type VelocityOnAxis = [Int]
type StepFun
   = PositionOnAxis -> VelocityOnAxis -> (PositionOnAxis, VelocityOnAxis)

parseInput :: Monad m => Text -> ExceptT String m [CoordsOfMoon]
parseInput input = do
  moons <- except . first errorBundlePretty $ parse parseAll "" input
  return $ map (map snd) moons

gravity :: PositionOnAxis -> VelocityOnAxis -> VelocityOnAxis
gravity pos vel =
  let diff :: Int -> Int
      diff moon = sum $ subtract 1 . fromEnum . compare (pos !! moon) <$> pos
      diffs = diff <$> [0..3]
   in zipWith (-) vel diffs

velocity :: PositionOnAxis -> VelocityOnAxis -> PositionOnAxis
velocity = zipWith (+)

step :: StepFun
step pos vel =
  let newVel = gravity pos vel
      newPos = velocity pos newVel
   in (newPos, newVel)

stepN :: Int -> StepFun
stepN n pos vel = snd $ until ((>= n) . fst) go (0,(pos,vel))
  where
    go (c,arg) = (succ c, uncurry step arg)

stepTillEq :: (PositionOnAxis, VelocityOnAxis) -> Int
stepTillEq init = fst $ until (\(n, st) -> n > 0 && st == init) go (0, init)
  where
    go (force -> (!c, !arg)) = (succ c, uncurry step arg)

energy :: [[Int]] -> [Int]
energy byAxis =
  let byMoon = transpose byAxis
   in sum . map abs <$> byMoon

part1 :: [CoordsOfMoon] -> Int
part1 coords =
  let coordsByAxis = transpose coords
      initVel = replicate 4 [0,0,0,0]
      after1000 = uncurry (stepN 1000) <$> zip coordsByAxis initVel
      (pos, vel) = unzip after1000
   in sum $ zipWith (*) (energy pos) (energy vel)

part2 :: [CoordsOfMoon] -> Int
part2 coords =
  let coordsByAxis = transpose coords
      initVel = replicate 4 [0,0,0,0]
   in foldr lcm 1 $ stepTillEq <$> zip coordsByAxis initVel

solve12 :: Handle -> IO String
solve12 handle = fmap (either id id) . runExceptT $ do
  input <- lift $ Text.IO.hGetContents handle
  parsed <- parseInput input
  let part1out = part1 parsed
  let part2out = part2 parsed
  return . unlines . map show $ [part1out, part2out]

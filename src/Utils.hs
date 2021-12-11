{-# LANGUAGE TupleSections #-}

module Utils where

import Data.Array.IArray
import Data.Char
import Data.Either
import qualified Data.Map.Strict as M
import Data.List
import Text.Parsec

-- parsing
----------

type Parser = Parsec String ()

doAParse :: Parser p -> p -> String -> p
doAParse p def = fromRight def . parse p ""

num :: (Num p) => Parser p
num = fromIntegral . read <$> many1 digit

sepByCommas :: Parser p -> Parser [p]
sepByCommas p = sepBy p (char ',')

sepEndByNewLines :: Parser p -> Parser [p]
sepEndByNewLines p = sepEndBy p endOfLine

sepByNewLines :: Parser p -> Parser [p]
sepByNewLines p = sepBy p endOfLine

sepPair :: Parser p -> Parser sep -> Parser (p, p)
sepPair p sep = (,) <$> (p <* sep) <*> p

-- 2-dimensions
---------------

type Position = (Int, Int)
type Grid = Array Position

singleDigit :: Char -> Int
singleDigit c = ord c - ord '0'

parseBlockOfNums :: String -> Grid Int
parseBlockOfNums s = listArray ((1, 1), (h, w)) $ concat parseResult
  where
    h           = length parseResult
    w           = length $ head parseResult
    parseResult = map (map singleDigit) $ lines s

adjacents :: (Position, Position) -> Position -> [Position]
adjacents bs (y, x) =
  [ (y', x')
  | y' <- [y - 1 .. y + 1]
  , x' <- [x - 1 .. x + 1]
  , (y', x') /= (y, x)
  , inRange bs (y', x')
  ]

-- Other misc stuff
-------------------

-- Takes a list of values, and returns a map relating each value in the list to
-- the number of times it appeared in the list
counter :: (Ord a) => [a] -> M.Map a Int
counter coll = M.fromListWith (+) $ map (, 1) coll

unions :: (Eq a) => [[a]] -> [a]
unions = foldr union []

simulateN :: Int -> (a -> a) -> a -> a
simulateN n tick start = iterate tick start !! n

simulateUntil :: (a -> Bool) -> (a -> a) -> a -> a
simulateUntil stop tick start = head $ dropWhile stop $ iterate tick start

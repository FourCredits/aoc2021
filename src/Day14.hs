module Day14 where

import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Debug.Trace
import Text.Parsec

import Utils

type Polymer = String
-- type Rule = ((Char, Char), Char)
type Input = (Polymer, M.Map (Char, Char) Char)

parser :: String -> Input
parser = doAParse p ([], M.empty)
  where
    p = sepTwo polymer rules (endOfLine *> endOfLine)
    rules = M.fromList <$> sepEndByNewLines rule
    rule = do
      a <- letter
      b <- letter
      _ <- string " -> "
      c <- letter
      return ((a, b), c)
    polymer = many letter

part1 :: Input -> Int
part1 = uncurry (-) . extrema . solve 10

part2 :: Input -> Int
part2 = uncurry (-) . extrema . solve 40

type CountMap = M.Map Char Int
type Memo = M.Map (Char, Char, Int) CountMap
type Rules = M.Map (Char, Char) Char

solve :: Int -> Input -> CountMap
solve n (polymer, rules) =
  M.adjust (+ 1) (last polymer) $
  fst $ foldr f (M.empty, M.empty) $ zip polymer (tail polymer)
  where
    f :: (Char, Char) -> (CountMap, Memo) -> (CountMap, Memo)
    f (c1, c2) (map, memo) =
      let (result, memo') = count n memo (c1, c2)
       in ( M.adjust (subtract 1) c2 $ joinCountMap map result
          , M.union memo memo')
    count :: Int -> Memo -> (Char, Char) -> (CountMap, Memo)
    count n memo (c1, c2)
      | Just result <- memo M.!? (c1, c2, n) = (result, memo)
    count 0 memo (c1, c2) = (M.fromListWith (+) [(c1, 1), (c2, 1)], memo)
    count n memo (c1, c2) = (result, memo''')
      where
        c            = rules M.! (c1, c2)
        (r1, memo')  = count (n - 1) memo (c1, c)
        (r2, memo'') = count (n - 1) memo' (c, c2)
        result       = M.adjust (subtract 1) c $ joinCountMap r1 r2
        memo'''      = M.insert (c1, c2, n) result memo''

joinCountMap :: CountMap -> CountMap -> CountMap
joinCountMap = M.unionWith (+)

extrema :: Foldable f => f Int -> (Int, Int)
extrema map = (maximum map, minimum map)

-- TODO: delete test data
example :: Input
example = (polymer, rules)
polymer = "NNCB"
rules = 
  M.fromList
    [ (('C', 'H'), 'B')
    , (('H', 'H'), 'N')
    , (('C', 'B'), 'H')
    , (('N', 'H'), 'C')
    , (('H', 'B'), 'C')
    , (('H', 'C'), 'B')
    , (('H', 'N'), 'C')
    , (('N', 'N'), 'C')
    , (('B', 'H'), 'H')
    , (('N', 'C'), 'B')
    , (('N', 'B'), 'B')
    , (('B', 'N'), 'B')
    , (('B', 'B'), 'N')
    , (('B', 'C'), 'B')
    , (('C', 'C'), 'N')
    , (('C', 'N'), 'C')
    ]
{-
extrema :: Polymer -> (Int, Int)
extrema polymer = (mostCommon, leastCommon)
  where
    counts      = counter polymer
    leastCommon = minimum counts
    mostCommon  = maximum counts

stepN :: Int -> Input -> Polymer
stepN n (polymer, rules) = simulateN n (\p -> step (p, rules)) polymer

step :: Input -> Polymer
step (polymer, rules) = interlace polymer $ zipWith inBetween polymer (tail polymer)
  where
    inBetween a b = rules M.! (a, b)

interlace :: [a] -> [a] -> [a]
interlace (a:as) (b:bs) = a:b:interlace as bs
interlace [] bs         = bs
interlace as []         = as

-}

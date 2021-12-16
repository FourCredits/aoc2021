module Day05 where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Utils.TwoD
import Utils.Misc
import Utils.Parsing

type Line = (Position, Position)

parser :: String -> [Line]
parser = map (readPos `splitBy` " -> ") . lines

expandLine :: Line -> [Position]
expandLine ((x1, y1), (x2, y2))
  | x1 == x2 && y1  < y2 = [(x1, y) | y <- [y1 .. y2]]
  | x1 == x2 && y1 >= y2 = [(x1, y) | y <- [y2 .. y1]]
  | y1 == y2 && x1  < x2 = [(x, y1) | x <- [x1 .. x2]]
  | y1 == y2 && x1 >= x2 = [(x, y1) | x <- [x2 .. x1]]
  | otherwise            = []

expandLine' :: Line -> [Position]
expandLine' line@((x1, y1), (x2, y2))
  | x1 == x2 || y1 == y2 = expandLine line
  | otherwise = take n $ zip (iterate (+ dirX) x1) (iterate (+ dirY) y1)
  where
    dirX = signum (x2 - x1)
    dirY = signum (y2 - y1)
    n    = abs (x2 - x1) + 1 -- This should be the same as abs (y2 - y1) + 1

countOverlaps :: (Ord a) => [a] -> Int
countOverlaps = length . M.filter (> 1) . counter

part1 :: [Line] -> Int
part1 = countOverlaps . concatMap expandLine

part2 :: [Line] -> Int
part2 = countOverlaps . concatMap expandLine'

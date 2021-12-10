module Day9 where

import Control.Monad
import Data.Array.IArray
import Data.Char
import Data.Function
import Data.List
import Data.Maybe 
import qualified Data.Map.Strict as M

import Utils

type Point = (Int, Int)
type Depth = Int
type Location = (Point, Depth)
type DepthMap = Array Point Depth

parser :: String -> DepthMap
parser s = listArray ((1, 1), (h, w)) $ concat parseResult
  where
    h = length parseResult
    w = length $ head parseResult
    parseResult = map (map (\c -> ord c - ord '0')) $ lines s

part1 :: DepthMap -> Int
part1 = sum . map ((+ 1) . snd) . lowPoints

part2 :: DepthMap -> Int
part2 = product . take 3 . sortBy (flip compare) . groupByLowPoints

adjacentPositions :: DepthMap -> Point -> [Location]
adjacentPositions dm (x, y) =
  [ (position, dm ! position)
  | position <- [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
  , inRange bds position
  ]
  where
    bds = bounds dm

isAdjacent :: Point -> Point -> Bool
isAdjacent (x, y) (x', y') =
  x == x' && abs (y' - y) < 1 || y == y' && abs (x' - x) < 1

lowPoints :: DepthMap -> [Location]
lowPoints dm = filter p $ assocs dm
  where
    p (pos, depth) = all ((> depth) . snd) $ adjacentPositions dm pos

groupByLowPoints :: DepthMap -> [Int]
groupByLowPoints dm =
  map snd $ M.toAscList $ counter $ mapMaybe flowsTo $ assocs dm
  where
    lps = lowPoints dm
    flowsTo loc@(pos, depth) 
      | depth == 9 = Nothing
      | loc `elem` lps = Just loc
      | lowers@(_:_) <- filter ((< depth) . snd) $ adjacentPositions dm pos =
        flowsTo $ minimumBy (compare `on` snd) lowers
      | otherwise = Nothing

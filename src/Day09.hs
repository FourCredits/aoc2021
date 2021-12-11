module Day09 where

import Control.Monad
import Data.Array.IArray
import Data.Char
import Data.Function
import Data.List
import Data.Maybe 
import qualified Data.Map.Strict as M

import Utils

type Depth = Int
type Location = (Position, Depth)
type DepthMap = Grid Depth

parser :: String -> DepthMap
parser = parseBlockOfNums

part1 :: DepthMap -> Int
part1 = sum . map ((+ 1) . snd) . lowPositions

part2 :: DepthMap -> Int
part2 = product . take 3 . sortBy (flip compare) . groupByLowPositions

adjacentPositions :: DepthMap -> Position -> [Location]
adjacentPositions dm (x, y) =
  [ (position, dm ! position)
  | position <- [(x, y + 1), (x, y - 1), (x + 1, y), (x - 1, y)]
  , inRange bds position
  ]
  where
    bds = bounds dm

isAdjacent :: Position -> Position -> Bool
isAdjacent (x, y) (x', y') =
  x == x' && abs (y' - y) < 1 || y == y' && abs (x' - x) < 1

lowPositions :: DepthMap -> [Location]
lowPositions dm = filter p $ assocs dm
  where
    p (pos, depth) = all ((> depth) . snd) $ adjacentPositions dm pos

groupByLowPositions :: DepthMap -> [Int]
groupByLowPositions dm =
  map snd $ M.toAscList $ counter $ mapMaybe flowsTo $ assocs dm
  where
    lps = lowPositions dm
    flowsTo loc@(pos, depth) 
      | depth == 9 = Nothing
      | loc `elem` lps = Just loc
      | lowers@(_:_) <- filter ((< depth) . snd) $ adjacentPositions dm pos =
        flowsTo $ minimumBy (compare `on` snd) lowers
      | otherwise = Nothing

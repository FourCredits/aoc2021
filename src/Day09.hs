module Day09 where

import Data.Array.IArray
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ord

import Utils.TwoD
import Utils.Misc

type Depth = Int
type Location = (Position, Depth)
type DepthMap = Grid Depth

parser :: String -> DepthMap
parser = mkGrid . map (map singleDigit) . lines

part1 :: DepthMap -> Int
part1 = sum . map ((+ 1) . snd) . lowPositions

part2 :: DepthMap -> Int
part2 = product . take 3 . sortBy (flip compare) . groupByLowPositions

adjacentPositions :: DepthMap -> Position -> [Location]
adjacentPositions dm pos = [(pos', dm ! pos') | pos' <- strictAdjacents bs pos]
  where bs = bounds dm

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
        flowsTo $ minimumBy (comparing snd) lowers
      | otherwise = Nothing

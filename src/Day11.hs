{-# LANGUAGE TupleSections #-}

module Day11 where

import Control.Arrow
import Data.Array.IArray
import Data.Char
import Data.List

import Utils.TwoD
import Utils.Misc

parser :: String -> Grid Int
parser = mkGrid . map (map digitToInt) . lines

part1 :: Grid Int -> Int
part1 = fst . simulateN 100 (\(n, g) -> first (+ n) $ step g) . (0, )

part2 :: Grid Int -> Int
part2 = length . takeWhile (any (/= 0) . snd) . iterate (step . snd) . (0,)

step :: Grid Int -> (Int, Grid Int)
step originalGrid = second resetFlashed $ flash [] $ fmap (+ 1) originalGrid
  where
    flash flashed grid
      | xs@(_:_) <- map fst $ filter (flashing flashed) $ assocs grid =
        flash (xs `union` flashed) (update grid $ concatMap (adjacents bs) xs)
      | otherwise = (length flashed, grid)
    resetFlashed grid = fmap (\v -> if v > 9 then 0 else v) grid
    flashing alreadyFlashed (pos, val) = pos `notElem` alreadyFlashed && val > 9
    update = foldr (\pos grid -> grid // [(pos, 1 + (grid ! pos))])
    bs = bounds originalGrid

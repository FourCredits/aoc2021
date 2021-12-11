module Day11 where

import Control.Arrow
import Data.Array.IArray
import Data.Char
import Data.Maybe

import Utils

-- TODO: extract this commonality from day 9

parser :: String -> Array Position Int
parser = parseBlockOfNums

part1 :: Array Position Int -> Int
part1 = simulateN 100

part2 :: Array Position Int -> Int
part2 = go 0
  where
    go n grid
      | all (== 0) grid = n
      | otherwise       = go (n + 1) (snd $ step grid)

simulateN :: Int -> Array Position Int -> Int
simulateN = go 0
  where
    go acc 0 grid = acc
    go acc n grid =
      let (flashed, grid') = step grid
       in go (acc + flashed) (n - 1) grid'

step :: Array Position Int -> (Int, Array Position Int)
step originalGrid = h $ g $ fmap (+ 1) originalGrid
  where
    bds = bounds originalGrid
    g = go []
      where
        go flashed grid =
          let octopuses = assocs grid
           in case map fst $
                   filter (\(p, v) -> p `notElem` flashed && v > 9) octopuses of
                [] -> grid
                xs ->
                  let flashed' = xs ++ flashed
                      toUpdate = concatMap adjacents xs
                   in go flashed' (update grid toUpdate)
        update = foldr (\pos grid -> grid // [(pos, 1 + (grid ! pos))])
        adjacents (y, x) =
          [ (y', x')
          | x' <- [x - 1 .. x + 1]
          , y' <- [y - 1 .. y + 1]
          , inRange bds (y', x')
          ]
    h grid =
      let flashed = filter ((> 9) . snd) $ assocs grid
       in (length flashed, grid // map (second (const 0)) flashed)

module Day06 where

import qualified Data.Map.Strict as M

import Utils.Parsing
import Utils.Misc

type Day = Int
type Fish = Int

parser :: String -> M.Map Fish Int
parser = counter . map read . commas

simulateFish :: Day -> M.Map Fish Int -> Int
simulateFish stop = go 0
  where
    go days fish
      | days == stop = sum $ M.elems fish
      | otherwise    = go (days + 1) $ M.foldrWithKey update M.empty fish
    update 0 v map = M.insertWith (+) 8 v $ M.insertWith (+) 6 v map
    update k v map = M.insertWith (+) (k - 1) v map

part1 :: M.Map Fish Int -> Int
part1 = simulateFish 80

part2 :: M.Map Fish Int -> Int
part2 = simulateFish 256

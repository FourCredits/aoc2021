{-# LANGUAGE TupleSections #-}

module Day06 where

import Data.Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Text.Parsec

import Utils

type Day = Integer

type Fish = Integer

parser :: String -> Map Fish Integer
parser = countFish . doAParse (sepByCommas num) []
  where
    countFish = M.fromListWith (+) . (zeroCounts ++) . map (, 1)
    -- We need to have a zero at 0 to 8 for changes to propagate correctly
    zeroCounts = [(i, 0) | i <- [0 .. 8]]

simulateFish :: Day -> Map Fish Integer -> Integer
simulateFish stop = go 0
  where
    go days fish
      | days == stop = sum $ M.elems fish
      | otherwise    = go (days + 1) $ M.foldrWithKey update M.empty fish
    update 0 v map = M.insertWith (+) 8 v $ M.insertWith (+) 6 v map
    update k v map = M.insertWith (+) (k - 1) v map

part1 :: Map Fish Integer -> Integer
part1 = simulateFish 80

part2 :: Map Fish Integer -> Integer
part2 = simulateFish 256

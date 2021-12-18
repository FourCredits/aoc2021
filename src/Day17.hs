-- {-# LANGUAGE TupleSections #-}

module Day17 where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Maybe
import Debug.Trace

import Utils.Misc
import Utils.Parsing
import Utils.TwoD

type Velocity = (Int, Int)
type Range    = (Int, Int)
type Area     = (Range, Range)
type State    = (Position, Velocity)
type Path     = [Position]

parser :: String -> Area
parser =
  twoOf .
  map (twoOf . map read . splitOn ".." . drop 2) .
  splitOn ", " .
  drop (length "target area: ")

inArea :: Area -> Position -> Bool
inArea ((minX, maxX), (minY, maxY)) (x, y) =
  x >= minX && x <= maxX && y >= minY && y <= maxY

simulate :: Area -> Velocity -> Maybe Int
simulate area@((minX, maxX), (minY, maxY)) = go (minBound :: Int) (0, 0)
  where
    go maxH pos@(pX, pY) vel@(vX, vY)
      | inArea area pos = Just maxH
      | pX > maxX || pY < minY = Nothing
      | otherwise = go (max maxH pY) (pX + vX, pY + vY) (vX - signum vX, vY - 1)

allVelocities :: Area -> [Maybe Int]
allVelocities area@((minX, maxX), (minY, maxY)) =
  map (simulate area) $ (,) <$> [1 .. maxX] <*> [minY .. 2000]

part1 :: Area -> Int
part1 = maximum . catMaybes . allVelocities

part2 :: Area -> Int
part2 = length . catMaybes . allVelocities

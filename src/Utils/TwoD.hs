module Utils.TwoD where

import Data.Array.IArray

import Utils.Parsing

type Position = (Int, Int)
type Grid = Array Position

adjacents :: (Position, Position) -> Position -> [Position]
adjacents bs (y, x) =
  [ (y', x')
  | y' <- [y - 1 .. y + 1]
  , x' <- [x - 1 .. x + 1]
  , (y', x') /= (y, x)
  , inRange bs (y', x')
  ]

strictAdjacents :: (Position, Position) -> Position -> [Position]
strictAdjacents bs (x, y) =
  filter (inRange bs) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

readPos :: String -> Position
readPos = read `splitBy` ","

mkGrid :: [[a]] -> Grid a
mkGrid g = listArray ((1, 1), (h, w)) $ concat g
  where
    h = length g
    w = length $ head g

module Day25 where

import Control.Monad
import Data.Array.IArray
import Data.Function
import Data.List
import Data.Maybe

import Utils.TwoD
import Utils.Misc

data SeaCucumber = None | South | East deriving (Eq)
newtype Map = Map {getMap :: Array Position SeaCucumber} deriving (Eq)

parser :: String -> Map
parser = Map . mkGrid . map (map translate) . lines
  where
    translate '.' = None
    translate '>' = East
    translate 'v' = South
    translate _   = undefined

part1 :: Map -> Int
part1 map = 1 + fromJust (findIndex id $ zipWith (==) maps (tail maps))
  where maps = iterate step map

step :: Map -> Map
step = stepSouth . stepEast

stepEast :: Map -> Map
stepEast (Map map) = Map $ map // do
    p <- indices map
    guard $ (map ! p == East) && (map ! next p == None)
    [(p, None), (next p, East)]
  where
    next (y, x) = (y, (x + 1) `wrap` maxX)
    (_, (_, maxX)) = bounds map

stepSouth :: Map -> Map
stepSouth (Map map) = Map $ map // do
    p <- indices map
    guard $ (map ! p == South) && (map ! next p == None)
    [(p, None), (next p, South)]
  where
    next (y, x) = ((y + 1) `wrap` maxY, x)
    (_, (maxY, _)) = bounds map

instance Show Map where
  show =
    unlines .
    map (concatMap (show . snd)) .
    groupBy ((==) `on` (fst . fst)) . assocs . getMap

instance Show SeaCucumber where
  show None  = "."
  show South = "v"
  show East  = ">"

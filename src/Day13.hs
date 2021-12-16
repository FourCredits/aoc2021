module Day13 where

import Data.Bifunctor
import Data.Bool
import Data.List

import Utils.TwoD

data Fold
  = Horizontal Int
  | Vertical   Int
  deriving (Eq, Show)

type Input = ([Position], [Fold])

parser :: String -> Input
parser = bimap (map readPos) (map readFold . tail) . break null . lines
  where
    readFold :: String -> Fold
    readFold s =
      case drop l s of
          ('y':'=':n) -> Horizontal (read n)
          ('x':'=':n) -> Vertical   (read n)
          _           -> undefined
    l = length "fold along "

part1 :: Input -> Int
part1 (positions, folds) = length $ performFold positions $ head folds

part2 :: Input -> IO ()
part2 (positions, folds) = drawDots $ foldl' performFold positions folds

biggest :: [Position] -> Position
biggest = bimap maximum maximum . unzip

smallest :: [Position] -> Position
smallest = bimap minimum minimum . unzip

performFold :: [Position] -> Fold -> [Position]
performFold ps (Horizontal y) = nub $ map (second f) ps
  where
    f y'
      | y' < y = y'
      | y' > y = 2 * y - y'
      | otherwise = undefined
performFold ps (Vertical x) = nub $ map (first f) ps
  where
    f x'
      | x' < x = x'
      | x' > x = 2 * x - x'
      | otherwise = undefined

drawDots :: [Position] -> IO ()
drawDots dots =
  putStrLn $
  unlines
    [ [bool ' ' 'M' ((x, y) `elem` dots) | x <- [minX .. maxX]]
    | y <- [minY .. maxY]
    ]
  where
    (minX, minY) = smallest dots
    (maxX, maxY) = biggest dots

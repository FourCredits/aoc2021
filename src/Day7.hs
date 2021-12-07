module Day7 where

import Utils

parser :: String -> [Int]
parser = doAParse (sepByCommas num) []

simpleFuelCost :: [Int] -> Int -> Int
simpleFuelCost positions align = sum $ map (abs . subtract align) positions

triangle :: Int -> Int
triangle n = (n * (n + 1)) `div` 2

updatedFuelCost :: [Int] -> Int -> Int
updatedFuelCost positions align =
  sum $ map (triangle . abs . subtract align) positions

smallestCost :: ([Int] -> Int -> Int) -> [Int] -> Int
smallestCost strategy positions =
  minimum $ map (strategy positions) [minimum positions .. maximum positions]

part1 :: [Int] -> Int
part1 = smallestCost simpleFuelCost

part2 :: [Int] -> Int
part2 = smallestCost updatedFuelCost

module Day3 where

import Control.Applicative
import Data.Bits
import Data.Function
import Data.List

part1 :: [String] -> Int
part1 ns = gamma * epsilon
  where
    gamma   = foldBits $ map moreCommon $ transpose ns
    epsilon = foldBits $ map lessCommon $ transpose ns

part2 :: [String] -> Int
part2 ns = co2Scrubber * o2Generator
  where
    o2Generator = foldBits (findBest moreCommon ns)
    co2Scrubber = foldBits (findBest lessCommon ns)

foldBits :: String -> Int
foldBits = foldl' f 0
  where
    f n '1' = shiftL n 1 .|. 1
    f n '0' = shiftL n 1 .|. 0

moreCommon :: [Char] -> Char
moreCommon line =
  if (length line - 1) `div` 2 < length (filter (== '1') line)
    then '1'
    else '0'

lessCommon :: [Char] -> Char
lessCommon line =
  if (length line - 1) `div` 2 < length (filter (== '1') line)
    then '0'
    else '1'

findBest :: ([Char] -> Char) -> [String] -> String
findBest strategy = go 0
  where
    go _ [option] = option
    go n options =
      go (n + 1) $
      filter
        (\option -> (option !! n) == strategy (transpose options !! n))
        options

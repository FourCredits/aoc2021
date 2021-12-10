module Main where

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
import qualified Day8 as D8
import qualified Day9 as D9
import qualified Day10 as D10

runDay :: (Show b) => FilePath -> (String -> a) -> (a -> b) -> (a -> b) -> IO ()
runDay filepath parse part1 part2 = do
  input <- parse <$> readFile filepath
  print $ part1 input
  print $ part2 input

main :: IO ()
main = do
  runDay  "resources/1.txt" D1.parser  D1.part1  D1.part2
  runDay  "resources/2.txt" D2.parser  D2.part1  D2.part2
  runDay  "resources/3.txt"     lines  D3.part1  D3.part2
  runDay  "resources/4.txt" D4.parser  D4.part1  D4.part2
  runDay  "resources/5.txt" D5.parser  D5.part1  D5.part2
  runDay  "resources/6.txt" D6.parser  D6.part1  D6.part2
  runDay  "resources/7.txt" D7.parser  D7.part1  D7.part2
  runDay  "resources/8.txt" D8.parser  D8.part1  D8.part2
  runDay  "resources/9.txt" D9.parser  D9.part1  D9.part2
  runDay "resources/10.txt"     lines D10.part1 D10.part2

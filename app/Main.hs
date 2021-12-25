module Main where

import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06
import qualified Day07 as D07
import qualified Day08 as D08
import qualified Day09 as D09
import qualified Day10 as D10
import qualified Day11 as D11
import qualified Day12 as D12
import qualified Day13 as D13
import qualified Day14 as D14
import qualified Day15 as D15
import qualified Day16 as D16
import qualified Day17 as D17
import qualified Day18 as D18
import qualified Day20 as D20

runDay ::
     (Show b, Show c)
  => FilePath
  -> (String -> a)
  -> (a -> b)
  -> (a -> c)
  -> IO ()
runDay filepath parser part1 part2 = do
  input <- parser <$> readFile filepath
  print $ part1 input
  print $ part2 input

runDayIO :: FilePath -> (String -> a) -> (a -> IO b) -> (a -> IO c) -> IO ()
runDayIO filepath parser part1 part2 = do
  input <- parser <$> readFile filepath
  part1 input
  part2 input
  return ()

main :: IO ()
main = do
  runDay   "resources/01.txt" D01.parser          D01.part1  D01.part2
  runDay   "resources/02.txt" D02.parser          D02.part1  D02.part2
  runDay   "resources/03.txt"      lines          D03.part1  D03.part2
  runDay   "resources/04.txt" D04.parser          D04.part1  D04.part2
  runDay   "resources/05.txt" D05.parser          D05.part1  D05.part2
  runDay   "resources/06.txt" D06.parser          D06.part1  D06.part2
  runDay   "resources/07.txt" D07.parser          D07.part1  D07.part2
  runDay   "resources/08.txt" D08.parser          D08.part1  D08.part2
  runDay   "resources/09.txt" D09.parser          D09.part1  D09.part2
  runDay   "resources/10.txt"      lines          D10.part1  D10.part2
  runDay   "resources/11.txt" D11.parser          D11.part1  D11.part2
  runDay   "resources/12.txt" D12.parser          D12.part1  D12.part2
  runDayIO "resources/13.txt" D13.parser (print . D13.part1) D13.part2
  runDay   "resources/14.txt" D14.parser          D14.part1  D14.part2
  runDay   "resources/15.txt" D15.parser          D15.part1  D15.part2
  runDay   "resources/16.txt" D16.parser          D16.part1  D16.part2
  runDay   "resources/17.txt" D17.parser          D17.part1  D17.part2
  runDay   "resources/18.txt" D18.parser          D18.part1  D18.part2
  runDay   "resources/20.txt" D20.parser          D20.part1  D20.part2

module Day15Spec where

import Test.HUnit

import Common
import Day15

day15 :: Test
day15 = "day 15 tests" ~: TestList [example, real]

example =
  TestList
    ["part 1" ~: 40 @=? part1 testData, "part 2" ~: 315 @=? part2 testData]

real = realDeal "resources/15.txt" parser (part1, 583) (part2, 2927)

testData =
  parser $
  unlines
    [ "1163751742"
    , "1381373672"
    , "2136511328"
    , "3694931569"
    , "7463417111"
    , "1319128137"
    , "1359912421"
    , "3125421639"
    , "1293138521"
    , "2311944581"
    ]

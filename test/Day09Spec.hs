module Day09Spec where

import Data.Array.IArray
import Test.HUnit

import Common
import Day09

day09 :: Test
day09 = "day 9 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: testData @=? parser parserExample

example =
  TestList
    [ "part 1" ~: 15 @=? part1 testData
    , "part 2" ~: 1134 @=? part2 testData
    ]

real = realDeal "resources/09.txt" parser (part1, 452) (part2, 1263735)

testData =
  listArray
    ((1, 1), (5, 10))
    [ 2, 1, 9, 9, 9, 4, 3, 2, 1, 0
    , 3, 9, 8, 7, 8, 9, 4, 9, 2, 1
    , 9, 8, 5, 6, 7, 8, 9, 8, 9, 2
    , 8, 7, 6, 7, 8, 9, 6, 7, 8, 9
    , 9, 8, 9, 9, 9, 6, 5, 6, 7, 8
    ]
parserExample =
  unlines
    [ "2199943210"
    , "3987894921"
    , "9856789892"
    , "8767896789"
    , "9899965678"
    ]

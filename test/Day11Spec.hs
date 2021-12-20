module Day11Spec where

import Data.Array.IArray
import Test.HUnit

import Common
import Day11

day11 :: Test
day11 = "day 11 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: testData @=? parser parserInput 

example =
  TestList
    [ "part 1" ~:
      TestList
        [ "single step"     ~: (0, step1)           @=? step testData
        , "simple testData" ~: (9, simpletestData') @=? step simpletestData
        , "full testData"   ~: 1656                 @=? part1 testData
        ]
    , "part 2" ~: 195 @=? part2 testData
    ]

real = realDeal "resources/11.txt" parser (part1, 1675) (part2, 515)

simpletestData =
  listArray
    ((1, 1), (5, 5))
    [ 1, 1, 1, 1, 1
    , 1, 9, 9, 9, 1
    , 1, 9, 1, 9, 1
    , 1, 9, 9, 9, 1
    , 1, 1, 1, 1, 1
    ]
simpletestData' =
  listArray
    ((1, 1), (5, 5))
    [ 3, 4, 5, 4, 3
    , 4, 0, 0, 0, 4
    , 5, 0, 0, 0, 5
    , 4, 0, 0, 0, 4
    , 3, 4, 5, 4, 3
    ]
step1 =
  listArray
    ((1, 1), (10, 10))
    [ 6, 5, 9, 4, 2, 5, 4, 3, 3, 4
    , 3, 8, 5, 6, 9, 6, 5, 8, 2, 2
    , 6, 3, 7, 5, 6, 6, 7, 2, 8, 4
    , 7, 2, 5, 2, 4, 4, 7, 2, 5, 7
    , 7, 4, 6, 8, 4, 9, 6, 5, 8, 9
    , 5, 2, 7, 8, 6, 3, 5, 7, 5, 6
    , 3, 2, 8, 7, 9, 5, 2, 8, 3, 2
    , 7, 9, 9, 3, 9, 9, 2, 2, 4, 5
    , 5, 9, 5, 7, 9, 5, 9, 6, 6, 5
    , 6, 3, 9, 4, 8, 6, 2, 6, 3, 7
    ]
testData =
  listArray
    ((1, 1) , (10, 10))
    [ 5, 4, 8, 3, 1, 4, 3, 2, 2, 3
    , 2, 7, 4, 5, 8, 5, 4, 7, 1, 1
    , 5, 2, 6, 4, 5, 5, 6, 1, 7, 3
    , 6, 1, 4, 1, 3, 3, 6, 1, 4, 6
    , 6, 3, 5, 7, 3, 8, 5, 4, 7, 8
    , 4, 1, 6, 7, 5, 2, 4, 6, 4, 5
    , 2, 1, 7, 6, 8, 4, 1, 7, 2, 1
    , 6, 8, 8, 2, 8, 8, 1, 1, 3, 4
    , 4, 8, 4, 6, 8, 4, 8, 5, 5, 4
    , 5, 2, 8, 3, 7, 5, 1, 5, 2, 6
    ]
parserInput =
  unlines
    [ "5483143223"
    , "2745854711"
    , "5264556173"
    , "6141336146"
    , "6357385478"
    , "4167524645"
    , "2176841721"
    , "6882881134"
    , "4846848554"
    , "5283751526"
    ]

module Day05Spec where

import Test.HUnit

import Common
import Day05

day05 :: Test
day05 = "day 5 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: testData @=? parser parseEx

example =
  TestList
    [ "part 1" ~: 5  @=? part1 testData
    , "part 2" ~: 12 @=? part2 testData
    ]

real =
  realDeal "resources/05.txt" parser (part1, 7438) (part2, 21406)

testData :: [Line]
testData =
  [ ((0, 9), (5, 9))
  , ((8, 0), (0, 8))
  , ((9, 4), (3, 4))
  , ((2, 2), (2, 1))
  , ((7, 0), (7, 4))
  , ((6, 4), (2, 0))
  , ((0, 9), (2, 9))
  , ((3, 4), (1, 4))
  , ((0, 0), (8, 8))
  , ((5, 5), (8, 2))
  ]
parseEx =
  unlines
    [ "0,9 -> 5,9"
    , "8,0 -> 0,8"
    , "9,4 -> 3,4"
    , "2,2 -> 2,1"
    , "7,0 -> 7,4"
    , "6,4 -> 2,0"
    , "0,9 -> 2,9"
    , "3,4 -> 1,4"
    , "0,0 -> 8,8"
    , "5,5 -> 8,2"
    ]

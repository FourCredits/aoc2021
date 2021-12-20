module Day04Spec where

import Test.HUnit

import Common
import Day04

day04 :: Test
day04 = "day 4 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: (chosenNums, boards) @=? parser parseEx

example =
  TestList
    [ "part 1" ~: 4512 @=? part1 (chosenNums, boards)
    , "part 2" ~: 1924 @=? part2 (chosenNums, boards)
    ]

real = realDeal "resources/04.txt" parser (part1, 33348) (part2, 8112)

parseEx =
  unlines
    [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    , ""
    , "22 13 17 11  0"
    , " 8  2 23  4 24"
    , "21  9 14 16  7"
    , " 6 10  3 18  5"
    , " 1 12 20 15 19"
    , ""
    , " 3 15  0  2 22"
    , " 9 18 13 17  5"
    , "19  8  7 25 23"
    , "20 11 10 24  4"
    , "14 21 16 12  6"
    , ""
    , "14 21 17 24  4"
    , "10 16 15  9 19"
    , "18  8 23 26 20"
    , "22 11 13  6  5"
    , " 2  0 12  3  7"
    , ""
    ]

chosenNums =
  [ 7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22
  , 18, 20, 8, 19, 3, 26, 1 ]

boards =
  [ [ [(22, False), (13, False), (17, False), (11, False), (0, False)]
    , [(8, False), (2, False), (23, False), (4, False), (24, False)]
    , [(21, False), (9, False), (14, False), (16, False), (7, False)]
    , [(6, False), (10, False), (3, False), (18, False), (5, False)]
    , [(1, False), (12, False), (20, False), (15, False), (19, False)]
    ]
  , [ [(3, False), (15, False), (0, False), (2, False), (22, False)]
    , [(9, False), (18, False), (13, False), (17, False), (5, False)]
    , [(19, False), (8, False), (7, False), (25, False), (23, False)]
    , [(20, False), (11, False), (10, False), (24, False), (4, False)]
    , [(14, False), (21, False), (16, False), (12, False), (6, False)]
    ]
  , [ [(14, False), (21, False), (17, False), (24, False), (4, False)]
    , [(10, False), (16, False), (15, False), (9, False), (19, False)]
    , [(18, False), (8, False), (23, False), (26, False), (20, False)]
    , [(22, False), (11, False), (13, False), (6, False), (5, False)]
    , [(2, False), (0, False), (12, False), (3, False), (7, False)]
    ]
  ]

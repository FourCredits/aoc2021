module Day06Spec where

import Test.HUnit
import qualified Data.Map as M

import Common
import Day06

day06 :: Test
day06 = "day 6 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: testData @=? parser parseInput

example =
  TestList
    [ "part 1 simple" ~: 26 @=? simulateFish 18 testData
    , "part 1" ~: 5934 @=? part1 testData
    , "part 2" ~: 26984457539 @=? part2 testData
    ]

real = realDeal "resources/06.txt" parser (part1, 360268) (part2, 1632146183902)

testData =
  M.fromAscList
    [(1, 1), (2, 1), (3, 2), (4, 1)]
parseInput = "3,4,3,1,2"


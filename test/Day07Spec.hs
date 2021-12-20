module Day07Spec where

import Test.HUnit

import Common
import Day07

day07 :: Test
day07 = "day 7 tests" ~: TestList [example, real]

example =
  TestList
    ["part 1" ~: 37 @=? part1 testData, "part 2" ~: 168 @=? part2 testData]

real = realDeal "resources/07.txt" parser (part1, 344735) (part2, 96798233)

testData = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

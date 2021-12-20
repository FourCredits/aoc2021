module Day17Spec where

import Test.HUnit

import Common
import Day17

day17 :: Test
day17 = "day 17 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: testData @=? parser parseEx

example =
  TestList
    ["part 1" ~: 45 @=? part1 testData, "part 2" ~: 112 @=? part2 testData]

real = realDeal "resources/17.txt" parser (part1, 30628) (part2, 0)

parseEx = "target area: x=20..30, y=-10..-5"
testData = ((20, 30), (-10, -5))

module Day02Spec where

import Test.HUnit

import Common
import Day02

day02 :: Test
day02 = "day 2 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: testData @=? parser parseEx

example =
  TestList
    ["part 1" ~: 150 @=? part1 testData, "part 2" ~: 900 @=? part2 testData]

real = realDeal "resources/02.txt" parser (part1, 1727835) (part2, 1544000595)

testData = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]
parseEx = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

module Day01Spec where

import Common
import Day01

day01 :: Test
day01 = "day 1" ~: TestList [example, real]

example =
  TestList ["part 1" ~: 7 @=? part1 testData, "part 2" ~: 5 @=? part2 testData]

real = realDeal "resources/01.txt" parser (part1, 1581) (part2, 1618)

testData = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

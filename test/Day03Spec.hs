module Day03Spec where

import Test.HUnit

import Common
import Day03

day03 :: Test
day03 = "day 3 tests" ~: TestList [example, real]

example =
  TestList
    ["part 1" ~: 198 @=? part1 testData, "part 2" ~: 230 @=? part2 testData]

real = realDeal "resources/03.txt" lines (part1, 4138664) (part2, 4273224)

testData =
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010"
  ]


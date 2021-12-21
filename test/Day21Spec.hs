module Day21Spec where

import Test.HUnit

import Common
import Day21

day21 :: Test
day21 = "day 21 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: testData @=? parser parseEx

example =
  TestList
    [ "part 1" ~: 739785          @=? part1 testData
    , "part 2" ~: 444356092776315 @=? part2 testData
    ]

real
 = realDeal "resources/21.txt" parser (part1, 678468) (part2, 131180774190079)

testData :: Input
testData = (4,8)

parseEx :: String
parseEx =
  "Player 1 starting position: 4\n\
  \Player 2 starting position: 8"

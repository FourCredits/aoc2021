module Day25Spec where

import Test.HUnit

import Common
import Day25

day25 :: Test
day25 = "day 25 tests" ~: TestList [singleStep, example, real]

singleStep = "single step" ~: step1 @=? step testData

example = "example" ~: 58 @=? part1 testData

-- day 25 doesn't have a part 2
real = realDeal "resources/25.txt" parser (part1, 530) (const 0, 0)

testData = parser
  "v...>>.vv>\n\
  \.vv>>.vv..\n\
  \>>.>v>...v\n\
  \>>v>>.>.v.\n\
  \v>v.vv.v..\n\
  \>.>>..v...\n\
  \.vv..>.>v.\n\
  \v.v..>>v.v\n\
  \....v..v.>"

step1 = parser
  "....>.>v.>\n\
  \v.v>.>v.v.\n\
  \>v>>..>v..\n\
  \>>v>v>.>.v\n\
  \.>v.v...v.\n\
  \v>>.>vvv..\n\
  \..v...>>..\n\
  \vv...>>vv.\n\
  \>.v.v..v.v"

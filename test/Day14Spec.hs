module Day14Spec where

import qualified Data.Map.Strict as M
import Test.HUnit

import Common
import Day14

day14 :: Test
day14 = "day 14 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: testData @=? parser parseEx

example =
  TestList
    [ "part 1" ~: 1588          @=? part1 testData
    , "part 2" ~: 2188189693529 @=? part2 testData
    ]

real = realDeal "resources/14.txt" parser (part1, 5656) (part2, 12271437788530)

testData :: Input
testData = (polymer, rules)
polymer = "NNCB"
rules = 
  M.fromList
    [ (('C', 'H'), 'B')
    , (('H', 'H'), 'N')
    , (('C', 'B'), 'H')
    , (('N', 'H'), 'C')
    , (('H', 'B'), 'C')
    , (('H', 'C'), 'B')
    , (('H', 'N'), 'C')
    , (('N', 'N'), 'C')
    , (('B', 'H'), 'H')
    , (('N', 'C'), 'B')
    , (('N', 'B'), 'B')
    , (('B', 'N'), 'B')
    , (('B', 'B'), 'N')
    , (('B', 'C'), 'B')
    , (('C', 'C'), 'N')
    , (('C', 'N'), 'C')
    ]
parseEx =
  unlines
    [ "NNCB"
    , ""
    , "CH -> B"
    , "HH -> N"
    , "CB -> H"
    , "NH -> C"
    , "HB -> C"
    , "HC -> B"
    , "HN -> C"
    , "NN -> C"
    , "BH -> H"
    , "NC -> B"
    , "NB -> B"
    , "BN -> B"
    , "BB -> N"
    , "BC -> B"
    , "CC -> N"
    , "CN -> C"
    ]

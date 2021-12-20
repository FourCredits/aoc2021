module Day13Spec where

import Test.HUnit

import Common
import Day13

day13 :: Test
day13 = "day 13 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: testData @=? parser parseEx

-- part 2 can't easily be tested
example = "part 1" ~: 17 @=? part1 testData

real =
  "real deal" ~: do
    input <- parser <$> readFile "resources/13.txt"
    618 @=? part1 input

testData = (dots, folds)
dots =
  [ (6,10)
  , (0,14)
  , (9,10)
  , (0,3)
  , (10,4)
  , (4,11)
  , (6,0)
  , (6,12)
  , (4,1)
  , (0,13)
  , (10,12)
  , (3,4)
  , (3,0)
  , (8,4)
  , (1,10)
  , (2,14)
  , (8,10)
  , (9,0)
  ]
folds = [Horizontal 7, Vertical 5]
parseEx =
  unlines
    [ "6,10"
    , "0,14"
    , "9,10"
    , "0,3"
    , "10,4"
    , "4,11"
    , "6,0"
    , "6,12"
    , "4,1"
    , "0,13"
    , "10,12"
    , "3,4"
    , "3,0"
    , "8,4"
    , "1,10"
    , "2,14"
    , "8,10"
    , "9,0"
    , ""
    , "fold along y=7"
    , "fold along x=5"
    ]

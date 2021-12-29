module Day24Spec where

import Test.HUnit
import qualified Data.Map.Strict as M

import Common
import Day24

day24 :: Test
day24 = "day 24 tests" ~: TestList [real]

real =
  realDeal
    "resources/24.txt"
    parser
    (part1, 99995969919326)
    (part2, 48111514719111)

module Main where

import Test.HUnit

import Day01Spec
import Day02Spec
import Day03Spec
import Day04Spec
import Day05Spec
import Day06Spec
import Day07Spec
import Day08Spec
import Day09Spec
import Day10Spec
import Day11Spec
import Day12Spec
import Day13Spec
import Day14Spec
import Day15Spec
import Day16Spec
import Day17Spec
import Day18Spec
import Day19Spec
import Day20Spec
import Day21Spec
import Day25Spec

main :: IO ()
main =
  runTestTTAndExit $
  TestList
    [ day01
    , day02
    , day03
    , day04
    , day05
    , day06
    , day07
    , day08
    , day09
    , day10
    , day11
    , day12
    , day13
    , day14
    , day16
    , day17
    , day18
    , day19
    , day20
    , day21
    , day25
    ]

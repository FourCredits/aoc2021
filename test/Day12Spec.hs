module Day12Spec where

import Test.HUnit

import Common
import Day12

day12 :: Test
day12 = "day 12 tests" ~: TestList [example, real]

example =
  TestList
    [ "part 1" ~:
      TestList
        [ "small"  ~: 10  @=? part1 small
        , "medium" ~: 19  @=? part1 medium
        , "large"  ~: 226 @=? part1 large
        ]
    , "part 2" ~:
      TestList
        [ "small"  ~: 36   @=? part2 small
        , "medium" ~: 103  @=? part2 medium
        , "large"  ~: 3509 @=? part2 large
        ]
    ]

real = realDeal "resources/12.txt" parser (part1, 5920) (part2, 155477)

small =
  parser $ unlines
    [ "start-A"
    , "start-b"
    , "A-c"
    , "A-b"
    , "b-d"
    , "A-end"
    , "b-end"
    ]
medium =
  parser $ unlines
    [ "dc-end"
    , "HN-start"
    , "start-kj"
    , "dc-start"
    , "dc-HN"
    , "LN-dc"
    , "HN-end"
    , "kj-sa"
    , "kj-HN"
    , "kj-dc"
    ]
large =
  parser $ unlines
    [ "fs-end"
    , "he-DX"
    , "fs-he"
    , "start-DX"
    , "pj-DX"
    , "end-zg"
    , "zg-sl"
    , "zg-pj"
    , "pj-he"
    , "RW-he"
    , "fs-DX"
    , "pj-RW"
    , "zg-RW"
    , "start-pj"
    , "he-WI"
    , "zg-he"
    , "pj-fs"
    , "start-RW"
    ]

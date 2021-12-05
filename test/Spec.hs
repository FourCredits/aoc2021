import Test.HUnit
import Control.Monad

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5

main :: IO Counts
main = runTestTT $ TestList [day1, day2, day3, day4, day5]

withInput :: FilePath -> (String -> a) -> (a -> Assertion) -> Assertion
withInput file parser action = action . parser =<< readFile file

day1 :: Test
day1 =
  "day 1 tests" ~:
  TestList
    [ "part 1 example" ~: 7 ~=? D1.part1 testData
    , "part 2 example" ~: 5 ~=? D1.part2 testData
    , "real deal" ~: withInput "resources/1.txt" D1.parse $ \i -> do
        1581 @=? D1.part1 i
        1618 @=? D1.part2 i
    ]
  where
    testData = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

day2 :: Test
day2 =
  "day 2 tests" ~:
  TestList
    [ "parsing" ~: parseEx ~=? D2.parse strEx
    , "part 1 example" ~: 150 ~=? D2.part1 parseEx
    , "part 2 example" ~: 900 ~=? D2.part2 parseEx
    , "real deal" ~: withInput "resources/2.txt" D2.parse $ \i -> do
        1727835 @=? D2.part1 i
        1544000595 @=? D2.part2 i
    ]
  where
    parseEx =
      [ (D2.Forward, 5) , (D2.Down, 5) , (D2.Forward, 8)
      , (D2.Up, 3) , (D2.Down, 8) , (D2.Forward, 2) ]
    strEx = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

day3 :: Test
day3 =
  "day 3 tests" ~:
  TestList
    [ "part 1 example" ~: 198 ~=? D3.part1 example
    , "part 2 example" ~: 230 ~=? D3.part2 example
    , "real deal" ~: withInput "resources/3.txt" D3.parse $ \i -> do
        4138664 @=? D3.part1 i
        4273224 @=? D3.part2 i
    ]
  where
    example =
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

day4 :: Test
day4 =
  "day 4 tests" ~:
  TestList
    [ "parsing" ~: (chosenNums, boards) @=? D4.parser parseEx
    , "part 1 example" ~: 4512 @=? D4.part1 chosenNums boards
    , "part 2 example" ~: 1924 @=? D4.part2 chosenNums boards
    , "real deal" ~: withInput "resources/4.txt" D4.parser $ \(ns, bs) -> do
        33348 @=? D4.part1 ns bs
        8112 @=? D4.part2 ns bs
    ]
  where
    parseEx =
      unlines
        [ "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
        , ""
        , "22 13 17 11  0"
        , " 8  2 23  4 24"
        , "21  9 14 16  7"
        , " 6 10  3 18  5"
        , " 1 12 20 15 19"
        , ""
        , " 3 15  0  2 22"
        , " 9 18 13 17  5"
        , "19  8  7 25 23"
        , "20 11 10 24  4"
        , "14 21 16 12  6"
        , ""
        , "14 21 17 24  4"
        , "10 16 15  9 19"
        , "18  8 23 26 20"
        , "22 11 13  6  5"
        , " 2  0 12  3  7"
        , ""
        ]
    chosenNums =
      [ 7, 4, 9, 5, 11, 17, 23, 2, 0, 14, 21, 24, 10, 16, 13, 6, 15, 25, 12, 22
      , 18, 20, 8, 19, 3, 26, 1 ]
    boards =
      [ [ [(22, False), (13, False), (17, False), (11, False), (0, False)]
        , [(8, False), (2, False), (23, False), (4, False), (24, False)]
        , [(21, False), (9, False), (14, False), (16, False), (7, False)]
        , [(6, False), (10, False), (3, False), (18, False), (5, False)]
        , [(1, False), (12, False), (20, False), (15, False), (19, False)]
        ]
      , [ [(3, False), (15, False), (0, False), (2, False), (22, False)]
        , [(9, False), (18, False), (13, False), (17, False), (5, False)]
        , [(19, False), (8, False), (7, False), (25, False), (23, False)]
        , [(20, False), (11, False), (10, False), (24, False), (4, False)]
        , [(14, False), (21, False), (16, False), (12, False), (6, False)]
        ]
      , [ [(14, False), (21, False), (17, False), (24, False), (4, False)]
        , [(10, False), (16, False), (15, False), (9, False), (19, False)]
        , [(18, False), (8, False), (23, False), (26, False), (20, False)]
        , [(22, False), (11, False), (13, False), (6, False), (5, False)]
        , [(2, False), (0, False), (12, False), (3, False), (7, False)]
        ]
      ]

day5 :: Test
day5 =
  "day 5 tests" ~:
  TestList
    [ "parsing" ~: example @=? D5.parser parseEx
    , "part 1 example" ~: 5 @=? D5.part1 example
    , "part 2 example" ~: 12 @=? D5.part2 example
    , "real deal" ~: withInput "resources/5.txt" D5.parser $ \i -> do
        7438 @=? D5.part1 i
        21406 @=? D5.part2 i
    ]
  where
    example =
      [ ((0, 9), (5, 9))
      , ((8, 0), (0, 8))
      , ((9, 4), (3, 4))
      , ((2, 2), (2, 1))
      , ((7, 0), (7, 4))
      , ((6, 4), (2, 0))
      , ((0, 9), (2, 9))
      , ((3, 4), (1, 4))
      , ((0, 0), (8, 8))
      , ((5, 5), (8, 2))
      ]
    parseEx =
      unlines
        [ "0,9 -> 5,9"
        , "8,0 -> 0,8"
        , "9,4 -> 3,4"
        , "2,2 -> 2,1"
        , "7,0 -> 7,4"
        , "6,4 -> 2,0"
        , "0,9 -> 2,9"
        , "3,4 -> 1,4"
        , "0,0 -> 8,8"
        , "5,5 -> 8,2"
        ]

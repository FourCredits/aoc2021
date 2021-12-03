import Test.HUnit
import Control.Monad

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3

main :: IO Counts
main = runTestTT $ TestList [day1, day2, day3]

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

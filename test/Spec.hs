import qualified Data.Map.Strict as M
import Test.HUnit

import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3
import qualified Day4 as D4
import qualified Day5 as D5
import qualified Day6 as D6
import qualified Day7 as D7
import qualified Day8 as D8

main :: IO Counts
main = runTestTT $ TestList [day1, day2, day3, day4, day5, day6, day7, day8]

withInput :: FilePath -> (String -> a) -> (a -> Assertion) -> Assertion
withInput file parser action = action . parser =<< readFile file

day1 :: Test
day1 =
  "day 1 tests" ~:
  TestList
    [ "part 1 example" ~: 7 @=? D1.part1 testData
    , "part 2 example" ~: 5 @=? D1.part2 testData
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
    [ "parsing" ~: parseEx @=? D2.parse strEx
    , "part 1 example" ~: 150 @=? D2.part1 parseEx
    , "part 2 example" ~: 900 @=? D2.part2 parseEx
    , "real deal" ~: withInput "resources/2.txt" D2.parse $ \i -> do
        1727835 @=? D2.part1 i
        1544000595 @=? D2.part2 i
    ]
  where
    parseEx =
      [D2.Forward 5, D2.Down 5, D2.Forward 8, D2.Up 3, D2.Down 8, D2.Forward 2]
    strEx = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

day3 :: Test
day3 =
  "day 3 tests" ~:
  TestList
    [ "part 1 example" ~: 198 @=? D3.part1 example
    , "part 2 example" ~: 230 @=? D3.part2 example
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

day6 :: Test
day6 =
  "day 6 tests" ~:
  TestList
    [ "parsing"               ~: example     @=? D6.parser parseInput
    , "part 1 example simple" ~: 26          @=? D6.simulateFish 18 example
    , "part 1 example"        ~: 5934        @=? D6.part1 example
    , "part 2 example"        ~: 26984457539 @=? D6.part2 example
    , "real deal" ~: withInput "resources/6.txt" D6.parser $ \i -> do
        360268 @=? D6.part1 i
        1632146183902 @=? D6.part2 i
    ]
  where
    example =
      M.fromAscList
        [(0, 0), (1, 1), (2, 1), (3, 2), (4, 1), (5, 0), (6, 0), (7, 0), (8, 0)]
    parseInput = "3,4,3,1,2"

day7 :: Test
day7 =
  "day 7 tests" ~:
  TestList
    [ "part 1 example" ~: 37 @=? D7.part1 example
    , "part 2 example" ~: 168 @=? D7.part2 example
    , "triangle numbers" ~: [1, 3, 6, 10] @=? map D7.triangle [1 .. 4]
    , "real deal" ~: withInput "resources/7.txt" D7.parser $ \i -> do
        344735 @=? D7.part1 i
        96798233 @=? D7.part2 i
    ]
  where
    example = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

day8 :: Test
day8 =
  "day 8 tests" ~:
  TestList
    [ "parsing - basic" ~: simpleExample @=? D8.parser singleLine
    , "parsing - example" ~: example @=? D8.parser exampleString
    , "part 1 example" ~: 26 @=? D8.part1 example
    , "seven seg translating" ~: 1234 @=?
      D8.readSevenSeg ["fc", "acdeg", "acdfg", "bcdf"]
    , "part 2 simple example" ~: 5353 @=? D8.part2 simpleExample
    , "part 2 example" ~: 61229 @=? D8.part2 example
    , "real deal" ~: withInput "resources/8.txt" D8.parser $ \i -> do
        344 @=? D8.part1 i
        1048410 @=? D8.part2 i
    ]
  where
    singleLine =
      "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"
    simpleExample =
      [ ( [ "acedgfb" , "cdfbe" , "gcdfa" , "fbcad" , "dab"
          , "cefabd" , "cdfgeb" , "eafb" , "cagedb" , "ab" ]
        , ["cdfeb", "fcadb", "cdfeb", "cdbaf"])
      ]
    exampleString =
      unlines
        [ "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
        , "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
        , "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
        , "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
        , "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
        , "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
        , "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
        , "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
        , "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
        , "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
        ]
    example =
      [ (["be", "cfbegad", "cbdgef", "fgaecd", "cgeb", "fdcge", "agebfd", "fecdb", "fabcd", "edb"] , ["fdgacbe", "cefdb", "cefbgd", "gcbe"])
      , (["edbfga", "begcd", "cbg", "gc", "gcadebf", "fbgde", "acbgfd", "abcde", "gfcbed", "gfec"] , ["fcgedb", "cgb", "dgebacf", "gc"])
      , (["fgaebd", "cg", "bdaec", "gdafb", "agbcfd", "gdcbef", "bgcad", "gfac", "gcb", "cdgabef"] , ["cg", "cg", "fdcagb", "cbg"])
      , (["fbegcd", "cbd", "adcefb", "dageb", "afcb", "bc", "aefdc", "ecdab", "fgdeca", "fcdbega"] , ["efabcd", "cedba", "gadfec", "cb"])
      , (["aecbfdg", "fbg", "gf", "bafeg", "dbefa", "fcge", "gcbea", "fcaegb", "dgceab", "fcbdga"] , ["gecf", "egdcabf", "bgf", "bfgea"])
      , (["fgeab", "ca", "afcebg", "bdacfeg", "cfaedg", "gcfdb", "baec", "bfadeg", "bafgc", "acf"] , ["gebdcfa", "ecba", "ca", "fadegcb"])
      , (["dbcfg", "fgd", "bdegcaf", "fgec", "aegbdf", "ecdfab", "fbedc", "dacgb", "gdcebf", "gf"] , ["cefg", "dcbef", "fcge", "gbcadfe"])
      , (["bdfegc", "cbegaf", "gecbf", "dfcage", "bdacg", "ed", "bedf", "ced", "adcbefg", "gebcd"] , ["ed", "bcgafe", "cdgba", "cbgef"])
      , (["egadfb", "cdbfeg", "cegd", "fecab", "cgb", "gbdefca", "cg", "fgcdab", "egfdb", "bfceg"] , ["gbdfcae", "bgc", "cg", "cgb"])
      , (["gcafb", "gcf", "dcaebfg", "ecagb", "gf", "abcdeg", "gaef", "cafbge", "fdbac", "fegbdc"] , ["fgae", "cfgab", "fg", "bagce"])
      ]

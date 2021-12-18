import Control.Monad
import Data.Array.IArray
import qualified Data.Map.Strict as M
import Test.HUnit

import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06
import qualified Day07 as D07
import qualified Day08 as D08
import qualified Day09 as D09
import qualified Day10 as D10
import qualified Day11 as D11
import qualified Day12 as D12
import qualified Day13 as D13
import qualified Day14 as D14
import qualified Day16 as D16
import qualified Day17 as D17
import qualified Day18 as D18

main :: IO ()
main = void $ runTestTT days
  where
    days =
      TestList
        [ day1
        , day2
        , day3
        , day4
        , day5
        , day6
        , day7
        , day8
        , day9
        , day10
        , day11
        , day12
        , day13
        , day14
        , day16
        , day17
        ]

realDeal ::
     (Eq b, Show b, Eq c, Show c)
  => FilePath
  -> (String -> a)
  -> (a -> b, b)
  -> (a -> c, c)
  -> Test
realDeal file parser (p1, a1) (p2, a2) =
  "real deal" ~: do
    input <- parser <$> readFile file
    a1 @=? p1 input
    a2 @=? p2 input

day1 :: Test
day1 =
  "day 1 tests" ~:
  TestList
    [ "part 1 example" ~: 7 @=? D01.part1 testData
    , "part 2 example" ~: 5 @=? D01.part2 testData
    , realDeal "resources/01.txt" D01.parser (D01.part1, 1581) (D01.part2, 1618)
    ]
  where
    testData = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

day2 :: Test
day2 =
  "day 2 tests" ~:
  TestList
    [ "parsing" ~: parseEx @=? D02.parser strEx
    , "part 1 example" ~: 150 @=? D02.part1 parseEx
    , "part 2 example" ~: 900 @=? D02.part2 parseEx
    , realDeal
        "resources/02.txt"
        D02.parser
        (D02.part1, 1727835)
        (D02.part2, 1544000595)
    ]
  where
    parseEx =
      [D02.Forward 5, D02.Down 5, D02.Forward 8, D02.Up 3, D02.Down 8, D02.Forward 2]
    strEx = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

day3 :: Test
day3 =
  "day 3 tests" ~:
  TestList
    [ "part 1 example" ~: 198 @=? D03.part1 example
    , "part 2 example" ~: 230 @=? D03.part2 example
    , realDeal "resources/03.txt" lines (D03.part1, 4138664) (D03.part2, 4273224)
    ]
  where
    example =
      [ "00100", "11110", "10110", "10111", "10101", "01111"
      , "00111", "11100", "10000", "11001", "00010", "01010" ]

day4 :: Test
day4 =
  "day 4 tests" ~:
  TestList
    [ "parsing"        ~: (chosenNums, boards) @=? D04.parser parseEx
    , "part 1 example" ~: 4512                 @=? D04.part1 (chosenNums, boards)
    , "part 2 example" ~: 1924                 @=? D04.part2 (chosenNums, boards)
    , realDeal "resources/04.txt" D04.parser (D04.part1, 33348) (D04.part2, 8112)
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
    [ "parsing"        ~: example @=? D05.parser parseEx
    , "part 1 example" ~: 5       @=? D05.part1  example
    , "part 2 example" ~: 12      @=? D05.part2  example
    , realDeal "resources/05.txt" D05.parser (D05.part1, 7438) (D05.part2, 21406)
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
    [ "parsing"               ~: example     @=? D06.parser parseInput
    , "part 1 example simple" ~: 26          @=? D06.simulateFish 18 example
    , "part 1 example"        ~: 5934        @=? D06.part1 example
    , "part 2 example"        ~: 26984457539 @=? D06.part2 example
    , realDeal
        "resources/06.txt"
        D06.parser
        (D06.part1, 360268)
        (D06.part2, 1632146183902)
    ]
  where
    example =
      M.fromAscList
        [(1, 1), (2, 1), (3, 2), (4, 1)]
    parseInput = "3,4,3,1,2"

day7 :: Test
day7 =
  "day 7 tests" ~:
  TestList
    [ "part 1 example"   ~: 37            @=? D07.part1 example
    , "part 2 example"   ~: 168           @=? D07.part2 example
    , "triangle numbers" ~: [1, 3, 6, 10] @=? map D07.triangle [1 .. 4]
    , realDeal
        "resources/07.txt"
        D07.parser
        (D07.part1, 344735)
        (D07.part2, 96798233)
    ]
  where
    example = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

day8 :: Test
day8 =
  "day 8 tests" ~:
  TestList
    [ "parsing" ~:
      TestList
        [ "parsing - basic"   ~: simpleExample @=? D08.parser singleLine
        , "parsing - example" ~: example       @=? D08.parser exampleString
        ]
    , "part 1"                ~: 26   @=? D08.part1        example
    , "seven seg translating" ~: 1234 @=? D08.readSevenSeg sevenSegEx
    , "part 2" ~:
      TestList
        [ "part 2 simple example" ~: 5353  @=? D08.part2 simpleExample
        , "part 2 example"        ~: 61229 @=? D08.part2 example
        ]
    , realDeal "resources/08.txt" D08.parser (D08.part1, 344) (D08.part2, 1048410)
    ]
  where
    sevenSegEx = ["fc", "acdeg", "acdfg", "bcdf"]
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

day9 :: Test
day9 =
  "day 9 tests" ~:
  TestList
    [ "parsing"        ~: example @=? D09.parser parserExample
    , "part 1 example" ~: 15      @=? D09.part1  example
    , "part 2 example" ~: 1134    @=? D09.part2  example
    , realDeal "resources/09.txt" D09.parser (D09.part1, 452) (D09.part2, 1263735)
    ]
  where
    example =
      listArray
        ((1, 1), (5, 10))
        [ 2, 1, 9, 9, 9, 4, 3, 2, 1, 0
        , 3, 9, 8, 7, 8, 9, 4, 9, 2, 1
        , 9, 8, 5, 6, 7, 8, 9, 8, 9, 2
        , 8, 7, 6, 7, 8, 9, 6, 7, 8, 9
        , 9, 8, 9, 9, 9, 6, 5, 6, 7, 8
        ]
    parserExample =
      unlines
        [ "2199943210"
        , "3987894921"
        , "9856789892"
        , "8767896789"
        , "9899965678"
        ]

day10 :: Test
day10 =
  "day 10 tests" ~:
  TestList
    [ "part 1" ~:
      TestList
        [ "full example" ~: 26397 @=? D10.part1 example
        , 1197    ~=? D10.scoreOfLine (example !! 2)
        , 3       ~=? D10.scoreOfLine (example !! 4)
        , 57      ~=? D10.scoreOfLine (example !! 5)
        , 3       ~=? D10.scoreOfLine (example !! 7)
        , 25137   ~=? D10.scoreOfLine (example !! 8)
        ]
    , "part 2" ~:
      TestList
        [ "full example" ~: 288957 @=? D10.part2  example
        , "median"       ~: 5      @=? D10.median [5, 1, 20, -200, 6]
        , "closing characters" ~:
          TestList
            [ "}}]])})]"  ~=? D10.closingChars (head example)
            , ")}>]})"    ~=? D10.closingChars (example !! 1)
            , "}}>}>))))" ~=? D10.closingChars (example !! 3)
            , "]]}}]}]}>" ~=? D10.closingChars (example !! 6)
            , "])}>"      ~=? D10.closingChars (example !! 9)
            ]
        ]
    , realDeal "resources/10.txt" lines (D10.part1, 345441) (D10.part2, 3235371166)
    ]
  where
    example =
      [ "[({(<(())[]>[[{[]{<()<>>"
      , "[(()[<>])]({[<{<<[]>>("
      , "{([(<{}[<>[]}>{[]{[(<()>"
      , "(((({<>}<{<{<>}{[]{[]{}"
      , "[[<[([]))<([[{}[[()]]]"
      , "[{[{({}]{}}([{[{{{}}([]"
      , "{<[[]]>}<{[{[{[]{()[[[]"
      , "[<(<(<(<{}))><([]([]()"
      , "<{([([[(<>()){}]>(<<{{"
      , "<{([{{}}[<[[[<>{}]]]>[]]"
      ]

day11 :: Test
day11 =
  "day 11 tests" ~:
  TestList
    [ "parsing" ~: example @=? D11.parser parserInput 
    , "part 1" ~:
      TestList
        [ "single step"        ~: (0, step1)          @=? D11.step example 
        , "simple example"     ~: (9, simpleExample') @=? D11.step simpleExample
        , "full example"       ~: 1656                @=? D11.part1 example
        ]
    , "part 2" ~: 195 @=? D11.part2 example
    , realDeal "resources/11.txt" D11.parser (D11.part1, 1675) (D11.part2, 515)
    ]
    where
      simpleExample =
        listArray
          ((1, 1), (5, 5))
          [ 1, 1, 1, 1, 1
          , 1, 9, 9, 9, 1
          , 1, 9, 1, 9, 1
          , 1, 9, 9, 9, 1
          , 1, 1, 1, 1, 1
          ]
      simpleExample' =
        listArray
          ((1, 1), (5, 5))
          [ 3, 4, 5, 4, 3
          , 4, 0, 0, 0, 4
          , 5, 0, 0, 0, 5
          , 4, 0, 0, 0, 4
          , 3, 4, 5, 4, 3
          ]
      step1 =
        listArray
          ((1, 1), (10, 10))
          [ 6, 5, 9, 4, 2, 5, 4, 3, 3, 4
          , 3, 8, 5, 6, 9, 6, 5, 8, 2, 2
          , 6, 3, 7, 5, 6, 6, 7, 2, 8, 4
          , 7, 2, 5, 2, 4, 4, 7, 2, 5, 7
          , 7, 4, 6, 8, 4, 9, 6, 5, 8, 9
          , 5, 2, 7, 8, 6, 3, 5, 7, 5, 6
          , 3, 2, 8, 7, 9, 5, 2, 8, 3, 2
          , 7, 9, 9, 3, 9, 9, 2, 2, 4, 5
          , 5, 9, 5, 7, 9, 5, 9, 6, 6, 5
          , 6, 3, 9, 4, 8, 6, 2, 6, 3, 7
          ]
      example =
        listArray
          ((1, 1) , (10, 10))
          [ 5, 4, 8, 3, 1, 4, 3, 2, 2, 3
          , 2, 7, 4, 5, 8, 5, 4, 7, 1, 1
          , 5, 2, 6, 4, 5, 5, 6, 1, 7, 3
          , 6, 1, 4, 1, 3, 3, 6, 1, 4, 6
          , 6, 3, 5, 7, 3, 8, 5, 4, 7, 8
          , 4, 1, 6, 7, 5, 2, 4, 6, 4, 5
          , 2, 1, 7, 6, 8, 4, 1, 7, 2, 1
          , 6, 8, 8, 2, 8, 8, 1, 1, 3, 4
          , 4, 8, 4, 6, 8, 4, 8, 5, 5, 4
          , 5, 2, 8, 3, 7, 5, 1, 5, 2, 6
          ]
      parserInput =
        unlines
          [ "5483143223"
          , "2745854711"
          , "5264556173"
          , "6141336146"
          , "6357385478"
          , "4167524645"
          , "2176841721"
          , "6882881134"
          , "4846848554"
          , "5283751526"
          ]

day12 :: Test
day12 =
  "day 12 tests" ~:
  TestList
    [ "part 1" ~:
      TestList
        [ "small"  ~: 10  @=? D12.part1 small
        , "medium" ~: 19  @=? D12.part1 medium
        , "large"  ~: 226 @=? D12.part1 large
        ]
    , "part 2" ~:
      TestList
        [ "small"  ~: 36   @=? D12.part2 small
        , "medium" ~: 103  @=? D12.part2 medium
        , "large"  ~: 3509 @=? D12.part2 large
        ]
    , realDeal
        "resources/12.txt"
        D12.parser
        (D12.part1, 5920)
        (D12.part2, 155477)
    ] 
  where
    small =
      D12.parser $ unlines
        [ "start-A"
        , "start-b"
        , "A-c"
        , "A-b"
        , "b-d"
        , "A-end"
        , "b-end"
        ]
    medium =
      D12.parser $ unlines
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
      D12.parser $ unlines
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

day13 :: Test
day13 =
  "day 13 tests" ~:
  TestList
    [ "parsing" ~: example @=? D13.parser parseEx
    , "part 1" ~: 17 @=? D13.part1 example
    -- part 2 can't easily be tested
    , "real deal" ~: do
        input <- D13.parser <$> readFile "resources/13.txt" 
        618 @=? D13.part1 input
    ]
  where
    example = (dots, folds)
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
    folds = [D13.Horizontal 7, D13.Vertical 5]
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

day14 :: Test
day14 =
  "day 14 tests" ~:
  TestList
    [ "parsing" ~: example @=? D14.parser parseEx
    , "part 1 example" ~: 1588 @=? D14.part1 example
    , "part 2 example" ~: 2188189693529 @=? D14.part2 example
    , realDeal
        "resources/14.txt"
        D14.parser
        (D14.part1, 5656)
        (D14.part2, 12271437788530)
    ]
  where
    example = (polymer, rules)
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


day16 :: Test
day16 =
  "day 16 tests" ~:
  TestList
    [ "parsing" ~: example @=? D16.parser parseEx
    , "part 1 parsing" ~:
        (bitsParseEx, replicate 7 False) @=? bitsParser "38006F45291200"
    , "part 1" ~: 31 @=? part1 "A0016C880162017C3686B18A3D4780"
    , "part 2" ~:
      TestList
        [ "sum"          ~: 3  @=? part2 "C200B40A82"
        , "product"      ~: 54 @=? part2 "04005AC33890"
        , "minimum"      ~: 7  @=? part2 "880086C3E88112"
        , "maximum"      ~: 9  @=? part2 "CE00C43D881120"
        , "less than"    ~: 1  @=? part2 "D8005AC2A8F0"
        , "greater than" ~: 0  @=? part2 "F600BC2D8F"
        , "equal to"     ~: 0  @=? part2 "9C005AC2F8F0"
        , "big one"      ~: 1  @=? part2 "9C0141080250320F1802104A08"
        ]
    , realDeal
        "resources/16.txt"
        D16.parser
        (D16.part1, 991)
        (D16.part2, 1264485568252)
    ]
  where
    parseEx = "D2FE28"
    example =
      [ True, True, False, True, False, False, True, False, True, True, True
      , True, True, True, True, False, False, False, True, False, True, False
      , False , False ]
    bitsParseEx = D16.Operator 1 6 [D16.Literal 6 10, D16.Literal 2 20]
    bitsParser = D16.bitsParser . D16.parser
    part1 = D16.part1 . D16.parser
    part2 = D16.part2 . D16.parser

day17 :: Test
day17 =
  "day 17 tests" ~:
  TestList
    [ "parsing" ~: example @=? D17.parser parseEx
    , "part 1"  ~: 45      @=? D17.part1  example
    , "part 2"  ~: 112     @=? D17.part2  example
    , realDeal "resources/17.txt" D17.parser (D17.part1, 30628) (D17.part2, 0)
    ]
  where
    parseEx = "target area: x=20..30, y=-10..-5"
    example = ((20, 30), (-10, -5))

day18 :: Test
day18 =
  "day 18 tests" ~:
  TestList
    [ "parsing"   ~: parseEx        @=? show (D18.snailfish parseEx)
    , "addition"  ~: additionAnswer @=? uncurry D18.add addition
    , "magnitude" ~: 3488           @=? D18.magnitude magnitudeEx
    , "part 1"    ~: 4140           @=? D18.part1 bigEx
    , "part 2"    ~: 3993           @=? D18.part2 bigEx
    , realDeal "resources/18.txt" D18.parser (D18.part1, 3816) (D18.part2, 4819)
    ]
  where
    parseEx = "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
    explodeEx =
      zip
        (map D18.snailfish
           [ "[[[[[9,8],1],2],3],4]"
           , "[7,[6,[5,[4,[3,2]]]]]"
           , "[[6,[5,[4,[3,2]]]],1]"
           , "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"
           , "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
           ])
        (map D18.snailfish
           [ "[[[[0,9],2],3],4]"
           , "[7,[6,[5,[7,0]]]]"
           , "[[6,[5,[7,0]]],3]"
           , "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]"
           , "[[3,[2,[8,0]]],[9,[5,[7,0]]]]"
           ])
    addition =
      ( D18.snailfish "[[[[4,3],4],4],[7,[[8,4],9]]]"
      , D18.snailfish "[1,1]")
    additionAnswer = D18.snailfish "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"
    magnitudeEx =
      D18.snailfish
        "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
    bigEx =
      D18.parser $
      unlines
        [ "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]"
        , "[[[5,[2,8]],4],[5,[[9,9],0]]]"
        , "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]"
        , "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]"
        , "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]"
        , "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]"
        , "[[[[5,4],[7,7]],8],[[8,3],8]]"
        , "[[9,3],[[9,9],[6,[4,9]]]]"
        , "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]"
        , "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
        ]
    bigExSum =
      D18.snailfish
        "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]"
    sumFishEx =
      zip
        (map
           (D18.parser . unlines)
           [ ["[1,1]", "[2,2]", "[3,3]", "[4,4]"]
           , ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]"]
           , ["[1,1]", "[2,2]", "[3,3]", "[4,4]", "[5,5]", "[6,6]"]
           , [ "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]"
             , "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"
             , "[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]"
             -- , "[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]"
             -- , "[7,[5,[[3,8],[1,4]]]]"
             -- , "[[2,[2,2]],[8,[8,1]]]"
             -- , "[2,9]"
             -- , "[1,[[[9,3],9],[[9,0],[0,7]]]]"
             -- , "[[[5,[7,4]],7],1]"
             -- , "[[[[4,2],2],6],[8,7]]"
             ]
           ])
        (map D18.snailfish
           [ "[[[[1,1],[2,2]],[3,3]],[4,4]]"
           , "[[[[3,0],[5,3]],[4,4]],[5,5]]"
           , "[[[[5,0],[7,4]],[5,5]],[6,6]]"
           -- , "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"
           , "[[[[6,7],[6,7]],[[7,7],[0,7]]],[[[8,7],[7,7]],[[8,8],[8,0]]]]"
           ])

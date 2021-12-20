module Day08Spec where

import Test.HUnit

import Common
import Day08

day08 :: Test
day08 = "day 8 tests" ~: TestList [parsing, example, sevenSeg, real]

parsing =
  TestList
    [ "parsing - basic" ~: simpleExample @=? parser singleLine
    , "parsing - example" ~: testData @=? parser exampleString
    ]

example =
  TestList
    [ "part 1" ~: 26 @=? part1 testData
    , "part 2" ~:
      TestList
        [ "simple example" ~: 5353 @=? part2 simpleExample
        , "example" ~: 61229 @=? part2 testData
        ]
    ]

sevenSeg = "seven seg translating" ~: 1234 @=? readSevenSeg sevenSegEx

real = realDeal "resources/08.txt" parser (part1, 344) (part2, 1048410)

-- data

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
testData =
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


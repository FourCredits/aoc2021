module Day10Spec where

import Test.HUnit

import Common
import Day10

day10 :: Test
day10 = "day 10 tests" ~: TestList [example, real]

example =
  TestList
    [ "part 1" ~:
      TestList
        [ "full testData" ~: 26397 @=? part1 testData
        , 1197  ~=? scoreOfLine (testData !! 2)
        , 3     ~=? scoreOfLine (testData !! 4)
        , 57    ~=? scoreOfLine (testData !! 5)
        , 3     ~=? scoreOfLine (testData !! 7)
        , 25137 ~=? scoreOfLine (testData !! 8)
        ]
    , "part 2" ~:
      TestList
        [ "full testData" ~: 288957 @=? part2 testData
        , "median"       ~:      5 @=? median [5, 1, 20, -200, 6]
        , "closing characters" ~:
          TestList
            [ "}}]])})]"  ~=? closingChars (head testData)
            , ")}>]})"    ~=? closingChars (testData !! 1)
            , "}}>}>))))" ~=? closingChars (testData !! 3)
            , "]]}}]}]}>" ~=? closingChars (testData !! 6)
            , "])}>"      ~=? closingChars (testData !! 9)
            ]
        ]
    ]

real =
  realDeal "resources/10.txt" lines (part1, 345441) (part2, 3235371166)

testData =
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

module Day16Spec where

import Test.HUnit

import Common
import Day16

day16 :: Test
day16 = "day 16 tests" ~: TestList [parsing, example, real]

parsing = "parsing" ~: testData @=? parser parseEx

example =
  TestList
    [ "part 1 parsing" ~: (bitsParseEx, replicate 7 False) @=?
      bitsParser (parser "38006F45291200")
    , "part 1" ~: 31 @=? part1 (parser "A0016C880162017C3686B18A3D4780")
    , "part 2" ~:
      TestList
        [ "sum"          ~: 3  @=? part2 (parser "C200B40A82")
        , "product"      ~: 54 @=? part2 (parser "04005AC33890")
        , "minimum"      ~: 7  @=? part2 (parser "880086C3E88112")
        , "maximum"      ~: 9  @=? part2 (parser "CE00C43D881120")
        , "less than"    ~: 1  @=? part2 (parser "D8005AC2A8F0")
        , "greater than" ~: 0  @=? part2 (parser "F600BC2D8F")
        , "equal to"     ~: 0  @=? part2 (parser "9C005AC2F8F0")
        , "big one"      ~: 1  @=? part2 (parser "9C0141080250320F1802104A08")
        ]
    ]

real = realDeal "resources/16.txt" parser (part1, 991) (part2, 1264485568252)

-- data

parseEx = "D2FE28"
testData =
  [ True, True, False, True, False, False, True, False, True, True, True
  , True, True, True, True, False, False, False, True, False, True, False
  , False , False ]
bitsParseEx = Operator 1 6 [Literal 6 10, Literal 2 20]

import Test.HUnit
import Control.Monad

import qualified Day1 as D1

main :: IO Counts
main = runTestTT $ TestList [day1]

withInput :: FilePath -> (String -> a) -> (a -> Assertion) -> Assertion
withInput file parser action = action =<< parser <$> readFile file

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

module Day24Spec where

import Test.HUnit
import qualified Data.Map.Strict as M

import Common
import Day24

day24 :: Test
day24 = "day 24 tests" ~: TestList [parsing, runningComp, real]

parsing =
  "parsing" ~: TestList $ zipWith (~=?) parseAnswers (map parser parseEx)

runningComp =
  "running the computer" ~:
  TestList
    [ TestList
        [ (-5) ~=? ((runComputer [ 5] (head parseAnswers) startComp) M.! X)
        ,   5  ~=? ((runComputer [-5] (head parseAnswers) startComp) M.! X)
        ]
    , TestList
        [ 0 ~=? ((runComputer [5, 14] (parseAnswers !! 1) startComp) M.! Z)
        , 1 ~=? ((runComputer [5, 15] (parseAnswers !! 1) startComp) M.! Z)
        ]
    , [0, 1, 0, 1] ~=? (M.elems (runComputer [5] (parseAnswers !! 2) startComp))
    ]

real = realDeal "resources/24.txt" parser (const 0, 0) (const 0, 0)

parseEx =
  [ "inp x\n\
    \mul x -1"
  , "inp z\n\
    \inp x\n\
    \mul z 3\n\
    \eql z x"
  , "inp w\n\
    \add z w\n\
    \mod z 2\n\
    \div w 2\n\
    \add y w\n\
    \mod y 2\n\
    \div w 2\n\
    \add x w\n\
    \mod x 2\n\
    \div w 2\n\
    \mod w 2"
  ]

parseAnswers =
  [ [Inp (Var X), Mul (Var X) (Literal (-1))]
  , [Inp (Var Z), Inp (Var X), Mul (Var Z) (Literal 3), Eql (Var Z) (Var X)]
  , [ Inp (Var W)
    , Add (Var Z) (Var W)
    , Mod (Var Z) (Literal 2)
    , Div (Var W) (Literal 2)
    , Add (Var Y) (Var W)
    , Mod (Var Y) (Literal 2)
    , Div (Var W) (Literal 2)
    , Add (Var X) (Var W)
    , Mod (Var X) (Literal 2)
    , Div (Var W) (Literal 2)
    , Mod (Var W) (Literal 2)
    ]
  ]

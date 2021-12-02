module Day2 where

import Data.List

data Direction = Forward | Up | Down deriving (Show, Eq)

type Instruction = (Direction, Int)

parse :: String -> [Instruction]
parse = map instruction . lines
  where
    instruction :: String -> Instruction
    instruction input =
      case words input of
        ["forward", n] -> (Forward, read n)
        ["up", n]      -> (Up, read n)
        ["down", n]    -> (Down, read n)

part1 :: [Instruction] -> Int
part1 = uncurry (*) . foldl' f (0, 0)
  where
    f (hPos, depth) (Forward, n) = (hPos + n, depth)
    f (hPos, depth) (Up, n)      = (hPos, depth - n)
    f (hPos, depth) (Down, n)    = (hPos, depth + n)

part2 :: [Instruction] -> Int
part2 = (\(h, d, a) -> h * d) . foldl' f (0, 0, 0)
  where
    f (h, d, a) (Forward, n) = (h + n, d + (a * n), a)
    f (h, d, a) (Up, n)      = (h, d, a - n)
    f (h, d, a) (Down, n)    = (h, d, a + n)

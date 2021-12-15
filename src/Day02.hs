module Day02 where

import Data.List

data Instruction
  = Forward Int
  | Up Int
  | Down Int
  deriving (Show, Eq)

parser :: String -> [Instruction]
parser = map instruction . lines
  where
    instruction s =
      case words s of
        ["forward", n] -> Forward (read n)
        ["up", n]      -> Up      (read n)
        ["down", n]    -> Down    (read n)
        _              -> undefined

part1 :: [Instruction] -> Int
part1 = uncurry (*) . foldl' f (0, 0)
  where
    f (hPos, depth) (Forward n) = (hPos + n, depth)
    f (hPos, depth) (Up n)      = (hPos, depth - n)
    f (hPos, depth) (Down n)    = (hPos, depth + n)

part2 :: [Instruction] -> Int
part2 = (\(h, d, a) -> h * d) . foldl' f (0, 0, 0)
  where
    f (h, d, a) (Forward n) = (h + n, d + (a * n), a)
    f (h, d, a) (Up n)      = (h, d, a - n)
    f (h, d, a) (Down n)    = (h, d, a + n)

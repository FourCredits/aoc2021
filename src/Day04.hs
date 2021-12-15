module Day04 where

import Data.Either
import Data.List

import Utils.Parsing

type Board = [[(Int, Bool)]]

parser :: String -> ([Int], [Board])
parser s = (map read $ splitOn "," chosen, map parseBoard boards)
  where
    (chosen:boards) = splitOn "\n\n" s
    parseBoard      = map (map (\n -> (read n, False)) . words) . lines

hasWon :: Board -> Bool
hasWon board = any wholeLine board || any wholeLine (transpose board)
  where wholeLine = all snd

update :: Int -> Board -> Board
update drawn = map (map f)
  where
    f (number, marked)
      | number == drawn = (number, True)
      | otherwise       = (number, marked)
    
calculateScore :: Board -> Int
calculateScore board = sum (map fst $ filter (not . snd) $ concat board)

part1 :: ([Int], [Board]) -> Int
part1 ([], _) = undefined
part1 (n:ns, boards) =
  case map (update n) boards of
    boards'
      | Just board <- find hasWon boards' -> n * calculateScore board
      | otherwise                         -> part1 (ns, boards')

part2 :: ([Int], [Board]) -> Int
part2 ([], _) = undefined
part2 (n:ns, boards) =
  case map (update n) boards of
    [board] | hasWon board -> n * calculateScore board
    boards'                -> part2 (ns, filter (not . hasWon) boards')

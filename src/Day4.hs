{-# LANGUAGE TupleSections #-}

module Day4 where

import Data.Either
import Data.List
import Text.Parsec

type Board = [[(Int, Bool)]]

mkBoard :: [[Int]] -> Board
mkBoard = map (map (, False))

parser :: String -> ([Int], [Board])
parser = fromRight ([], []) . parse input ""
  where
    input = (,) <$> chosenNumbers <*> boards
    chosenNumbers = sepBy num (char ',') <* count 2 endOfLine
    num = read <$> many1 digit
    boards = sepEndBy board endOfLine
    board = mkBoard <$> count 5 (line <* endOfLine)
    line = count 5 (many (char ' ') *> num)

hasWon :: Board -> Bool
hasWon board = any wholeLine board || any wholeLine (transpose board)
  where
    wholeLine = all snd

update :: Int -> Board -> Board
update drawnNumber = map (map f)
  where
    f (number, marked)
      | number == drawnNumber = (number, True)
      | otherwise = (number, marked)
    
calculateScore :: Board -> Int
calculateScore board = sum (map fst $ filter (not . snd) $ concat board)

part1 :: [Int] -> [Board] -> Int
-- part1 (n:ns) boards
--   | Just board <- find hasWon boards = calculateScore n board
--   | otherwise = part1 ns $ map (update n) boards

part1 (n:ns) boards =
  case map (update n) boards of
    boards'
      | Just board <- find hasWon boards' -> n * calculateScore board
      | otherwise -> part1 ns boards'

part2 :: [Int] -> [Board] -> Int
part2 (n:ns) boards =
  case map (update n) boards of
    [board] | hasWon board -> n * calculateScore board
    boards' -> part2 ns $ filter (not . hasWon) boards'

{-# LANGUAGE TupleSections #-}

module Day4 where

import Data.Either
import Data.List
import Text.Parsec

import Utils

type Board = [[(Int, Bool)]]

parser :: String -> ([Int], [Board])
parser = fromRight ([], []) . parse input ""
  where
    input         = (,) <$> chosenNumbers <*> boards
    chosenNumbers = sepByCommas num <* count 2 endOfLine
    boards        = sepEndByNewLines board
    board         = mkBoard <$> count 5 (line <* endOfLine)
    line          = count 5 (many (char ' ') *> num)
    mkBoard       = map (map (, False))

hasWon :: Board -> Bool
hasWon board = any wholeLine board || any wholeLine (transpose board)
  where
    wholeLine = all snd

update :: Int -> Board -> Board
update drawnNumber = map (map f)
  where
    f (number, marked)
      | number == drawnNumber = (number, True)
      | otherwise             = (number, marked)
    
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

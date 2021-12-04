{-# LANGUAGE TupleSections #-}

module Day4 where

import Data.List

type Board = [[(Int, Bool)]]

mkBoard :: [[Int]] -> Board
mkBoard = map (map (, False))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n as = take n as : chunksOf n (drop n as)

parse :: String -> ([Int], [Board])
parse s =
  let (cNumbers:_:boardLines) = lines s
      chosenNumbers = read $ '[' : cNumbers ++ "]"
      boardsChunks = map (take 5) $ chunksOf 6 boardLines
      boards = map (mkBoard . map (map read . words)) boardsChunks
   in (chosenNumbers, boards)

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

module Day15 where

import Control.Monad.State
import Data.Array.IArray
import qualified Data.Map.Strict as M
import Debug.Trace

import Utils.TwoD
import Utils.Misc

type Input = Grid Int
type Memo = M.Map (Position, Position) Int

parser :: String -> Input
parser = mkGrid . map (map singleDigit) . lines

part1 :: Input -> Int
part1 input = lowestRisk input end start
  where (start, end) = bounds input

part2 :: Input -> Int
part2 input = lowestRisk input (5 * x, 5 * y) start
  where (start, (x, y)) = bounds input

next :: Position -> Position -> [Position]
next (maxX, maxY) (x, y) =
  filter (\(x', y') -> x' <= maxX && y' <= maxY) [(x, y + 1), (x + 1, y)]

(!+) :: Input -> Position -> Int
(!+) grid = go
  where
    bs@(_ , (x', y')) = bounds grid
    go pos@(x, y)
      | x > x' = wrapAround 9 (1 + go (x - x', y))
      | y > y' = wrapAround 9 (1 + go (x, y - y'))
      | otherwise = grid ! pos

wrapAround :: Int -> Int -> Int
wrapAround base n = 1 + ((n - 1) `mod` base)

lowestRisk :: Input -> Position -> Position -> Int
lowestRisk grid end pos = evalState (go pos) M.empty
  where
    go :: Position -> State Memo Int
    go start
      | start == end = return 0
      | otherwise = do
        memo <- get
        case memo M.!? (start, end) of
          Just r -> return r
          Nothing -> do
            let positions' = next end start
            costs <- mapM (\pos' -> ((grid !+ pos') +) <$> go pos') positions'
            let result = minimum costs
            modify (M.insert (start, end) result)
            return result

example =
  parser $
  unlines
    [ "1163751742"
    , "1381373672"
    , "2136511328"
    , "3694931569"
    , "7463417111"
    , "1319128137"
    , "1359912421"
    , "3125421639"
    , "1293138521"
    , "2311944581"
    ]

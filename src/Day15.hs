{-# LANGUAGE TupleSections #-}

module Day15 where

import Control.Monad
import Control.Monad.ST
import Data.Array.IArray
import Data.Char
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as M
import Data.STRef
import qualified Data.Set as S

import Utils.TwoD

type Input = Grid Int
type Path = [Position]

parser :: String -> Input
parser = mkGrid . map (map digitToInt) . lines

part1 :: Input -> Int
part1 = lowestRisk

part2 :: Input -> Int
part2 = lowestRisk . expand (5, 5)

lowestRisk :: Input -> Int
lowestRisk grid = sum $ map (grid !) $ aStar grid start end
  where (start, end) = bounds grid

aStar :: Input -> Position -> Position -> Path
aStar grid start end@(endX, endY) =
  runST $ do
    let h (x, y)   = (endX - x) + (endY - y)
        neighbours = strictAdjacents (bounds grid)
    openSet  <- newSTRef (S.singleton (h start, start))
    cameFrom <- newSTRef M.empty
    gScore   <- newSTRef (M.singleton start 0)
    let search = do
          r <- S.minView <$> readSTRef openSet
          case r of
            Nothing -> pure []
            Just ((_, current), openSet')
              | current == end ->
                reconstructPath <$> readSTRef cameFrom <*> pure current
              | otherwise -> do
                writeSTRef openSet openSet'
                gs <- readSTRef gScore
                for_ (neighbours current) $ \neighbour -> do
                  let g' = grid ! neighbour + gs M.! current
                  when (g' < M.findWithDefault maxBound neighbour gs) $ do
                    modifySTRef openSet (S.insert (g' + h neighbour, neighbour))
                    modifySTRef cameFrom (M.insert neighbour current)
                    modifySTRef gScore (M.insert neighbour g')
                search
    search

reconstructPath :: M.Map Position Position -> Position -> Path
reconstructPath cameFrom = reverse . unfoldr (\p -> (p, ) <$> cameFrom M.!? p)

expand :: (Int, Int) -> Input -> Input
expand (a, b) grid = listArray bs' (map (grid !+) (range bs'))
  where
    bs@(start, (x, y)) = bounds grid
    bs' = (start, (x * a, y * b))

(!+) :: Input -> Position -> Int
(!+) grid (a, b) = ((grid ! (a `wrap` x, b `wrap` y)) + aOff + bOff) `wrap` 9
  where
    a `wrap` b = 1 + ((a - 1) `mod` b)
    bs@(start, (x, y)) = bounds grid
    aOff = (a - 1) `div` x
    bOff = (b - 1) `div` y

{-# LANGUAGE TupleSections #-}

module Day15 where

import Control.Monad.State
import Data.Array.IArray
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import Data.Ord
import Debug.Trace

import Utils.TwoD
import Utils.Misc

type Input = Grid Int
type Path = [Position]

parser :: String -> Input
parser = mkGrid . map (map digitToInt) . lines

data SState =
  SState
    { openSet  :: [Position] -- TODO: use better data type
    , cameFrom :: M.Map Position Position
    , gScore   :: M.Map Position Int
    , fScore   :: M.Map Position Int
    } deriving (Show, Eq, Ord)

initState :: Position -> (Position -> Int) -> SState
initState start h =
  SState
    { openSet = [start]
    , cameFrom = M.empty
    , gScore = M.singleton start 0
    , fScore = M.singleton start (h start)
    }

part1 :: Input -> Int
part1 = lowestRisk

part2 :: Input -> Int
part2 = lowestRisk . expand (5, 5)

lowestRisk :: Input -> Int
lowestRisk grid = sum $ map (grid !) $ aStar grid start end
  where (start, end) = bounds grid

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

printGrid :: Input -> IO ()
printGrid = mapM_ (print . map snd) . groupBy ((==) `on` (fst . fst)) . assocs

aStar :: Input -> Position -> Position -> Path
aStar grid start end@(endX, endY) = evalState search (initState start h)
  where
    h (x, y) = (endX - x) + (endY - y)
    neighbours = strictAdjacents (bounds grid)
    search = do
      os <- gets openSet
      if null os
        then pure []
        else do
          st <- get
          let current = minimumBy (comparing (fScore st M.!)) (openSet st)
          put $ st {openSet = delete current os}
          if current == end
            then return $ reconstructPath (cameFrom st) current
            else do
              for_ (neighbours current) $ \neighbour -> do
                gs <- gets gScore
                let tentativeGScore = gs M.! current + grid ! neighbour
                when (tentativeGScore < M.findWithDefault maxBound neighbour gs) $ do
                  modify
                    (\st ->
                      let cf' = M.insert neighbour current (cameFrom st)
                          gs' = M.insert neighbour tentativeGScore (gScore st)
                          fs' = M.insert neighbour (tentativeGScore + h neighbour) (fScore st)
                          os' = neighbour : openSet st
                       in st { cameFrom = cf', gScore = gs', fScore = fs', openSet = os' })
              search

reconstructPath :: M.Map Position Position -> Position -> Path
reconstructPath cameFrom = reverse . unfoldr (\pos -> (pos, ) <$> cameFrom M.!? pos)

smolInput :: Input
smolInput = parser "8"

mediumInput :: Input
mediumInput = parser "87\n43"

example :: Input
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

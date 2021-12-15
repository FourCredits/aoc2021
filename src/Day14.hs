module Day14 where

import Control.Monad.State
import Data.Bifunctor
import qualified Data.Map.Strict as M

import Utils.Parsing

type Polymer  = String
type CountMap = M.Map Char Int
type Memo     = M.Map (Char, Char, Int) CountMap
type Rules    = M.Map (Char, Char) Char
type Input    = (Polymer, Rules)

parser :: String -> Input
parser = second (M.fromList . map rule . lines) . twoOf . splitOn "\n\n"
  where rule s = let [[a, b], [c]] = splitOn " -> " s in ((a, b), c)

part1 :: Input -> Int
part1 = uncurry (-) . extrema . solve 10

part2 :: Input -> Int
part2 = uncurry (-) . extrema . solve 40

solve :: Int -> Input -> CountMap
solve n (polymer, rules) =
  M.adjust (+ 1) (last polymer) $
  evalState (foldM g M.empty $ zip polymer (tail polymer)) M.empty
  where
    g countMap (c1, c2) = joinMaps c2 countMap <$> count n c1 c2
    count :: Int -> Char -> Char -> State Memo CountMap
    count 0 c1 c2 = return $ M.fromListWith (+) [(c1, 1), (c2, 1)]
    count n c1 c2 = do
      memo <- get
      case memo M.!? (c1, c2, n) of
        Just r  -> return r
        Nothing -> do
          let c = rules M.! (c1, c2)
          r1 <- count (n - 1) c1 c
          r2 <- count (n - 1) c c2
          let result = joinMaps c r1 r2
          modify (M.insert (c1, c2, n) result)
          return result

joinMaps :: Char -> CountMap -> CountMap -> CountMap
joinMaps c m1 m2 = M.adjust (subtract 1) c $ M.unionWith (+) m1 m2

extrema :: Foldable f => f Int -> (Int, Int)
extrema map = (maximum map, minimum map)

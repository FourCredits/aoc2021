module Day23 where

import Control.Applicative
import Control.Arrow
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

import Utils.TwoD

data Amphipod = A | B | C | D deriving (Show, Eq, Ord, Enum)
type Diagram = M.Map Position Amphipod

energy :: Amphipod -> Int
energy = (10^) . fromEnum

room :: Amphipod -> Int
room = (* 2) . (+ 1) . fromEnum

end :: Diagram
end = fill $ [A .. D] >>= replicate 4

spaces, rooms, hallway :: [Int]
spaces = [1 .. 4]
rooms = [2, 4 .. 8]
hallway = filter (not . flip elem rooms) [0 .. 10]

fill :: [Amphipod] -> Diagram
fill = M.fromList . zip ((,) <$> rooms <*> spaces)

solve :: [Amphipod] -> Int
solve start = search S.empty (S.singleton (0, fill start))

part1, part2 :: Int
part1 = solve [C, D, A, A, C, D, B, B, A, B, C, C, B, A, D, D]
part2 = solve [C, D, D, D, C, C, B, D, A, B, A, B, B, A, C, A]

search :: S.Set Diagram -> S.Set (Int, Diagram) -> Int
search visited fringe
  | state == end = cost
  | S.member state visited = search visited next
  | otherwise =
    search
      (S.insert state visited)
      (S.union next $
       S.fromList $ M.assocs state >>= map (first (+ cost)) . steps state)
  where
    ((cost, state), next) = S.deleteFindMin fringe

steps :: Diagram -> (Position, Amphipod) -> [(Int, Diagram)]
steps state ((x, y), a)
  | y == 0 =
    maybe [] pure $
    if all available spaces
      then foldl1 (<|>) $ walk . (room a,) <$> reverse spaces
      else Nothing
  | x == room a
  , all available spaces = []
  | otherwise = mapMaybe walk $ (, 0) <$> hallway
  where
    walk (xt, yt) =
      let without = M.delete (x, y) state
       in if y == 0 && path without x xt yt || y > 0 && path without xt x y
            then Just
                   ( energy a * (abs (x - xt) + abs (y - yt))
                   , M.insert (xt, yt) a without)
            else Nothing
    available n = maybe True (== a) $ M.lookup (room a, n) state

path :: Diagram -> Int -> Int -> Int -> Bool
path state x xt yt =
  all (`M.notMember` state) $
  map (, 0) [min x xt .. max x xt] ++ map (xt, ) [1 .. yt]

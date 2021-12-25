module Day22 where

import Control.Arrow
import qualified Data.MultiSet as MS
import Data.Maybe

import Utils.Parsing

newtype Span = Span (Int, Int) deriving (Eq, Ord)
data Cuboid = Cuboid
  { spanX :: Span
  , spanY :: Span
  , spanZ :: Span
  } deriving (Eq, Ord)
data EngineAction = On | Off
data CuboidAction = CuboidAction EngineAction Cuboid
data EngineState = EngineState
  { add :: MS.MultiSet Cuboid
  , subtract :: MS.MultiSet Cuboid
  }
type Input = [CuboidAction]

parser :: String -> Input
parser = map line . lines
  where
    line = uncurry CuboidAction . (action *** cuboid) . twoOf . words
    action "on"  = On
    action "off" = Off
    action _     = undefined
    cuboid s =
      let [x, y, z] = map span $ splitOn "," s
       in Cuboid (Span x) (Span y) (Span z)
    span = (read *** read) . twoOf . splitOn ".." . drop 2

part1 :: Input -> Int
part1 = solve . take 20

part2 :: Input -> Int
part2 = solve

solve :: [CuboidAction] -> Int
solve =
  cardinalityEngine .
  foldl (\es cs -> gcEngine $ applyAction cs es) nullEngineState

cardinalityEngine :: EngineState -> Int
cardinalityEngine (EngineState adds subtracts) =
  sum (MS.map cardinalityCuboid adds) - sum (MS.map cardinalityCuboid subtracts)

mkSpan a b | b < a = error "Inverted span"
mkSpan a b = Span (a, b)

cardinalitySpan :: Span -> Int
cardinalitySpan (Span (a, b)) = b - a + 1

intersectSpan :: Span -> Span -> Maybe Span
intersectSpan (Span (a1, a2)) (Span (b1, b2))
  | max1 <= min2 = Just $ mkSpan max1 min2
  | otherwise    = Nothing
  where
    max1 = max a1 b1
    min2 = min a2 b2

cardinalityCuboid :: Cuboid -> Int
cardinalityCuboid (Cuboid x y z) = product $ map cardinalitySpan [x, y, z]

intersectCuboid :: Cuboid -> Cuboid -> Maybe Cuboid
intersectCuboid (Cuboid x1 y1 z1) (Cuboid x2 y2 z2) = do
  xspan <- intersectSpan x1 x2
  yspan <- intersectSpan y1 y2
  zspan <- intersectSpan z1 z2
  return $ Cuboid xspan yspan zspan

nullEngineState :: EngineState
nullEngineState = EngineState MS.empty MS.empty

gcEngine :: EngineState -> EngineState
gcEngine (EngineState adds subtracts) =
  EngineState (adds MS.\\ equals) (subtracts MS.\\ equals)
  where
    equals = MS.intersection adds subtracts

applyAction :: CuboidAction -> EngineState -> EngineState
applyAction (CuboidAction action c) (EngineState adds subtracts) =
  case action of
    On -> EngineState (MS.insert c $ MS.union subIntersects adds) (MS.union addIntersects subtracts)
    Off -> EngineState (MS.union subIntersects adds) (MS.union addIntersects subtracts)
  where
    addIntersects = MS.mapMaybe (intersectCuboid c) adds
    subIntersects = MS.mapMaybe (intersectCuboid c) subtracts

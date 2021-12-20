module Day19 where

import Data.Either
import Data.Function
import Data.List
import qualified Data.Set as S

import Utils.Misc
import Utils.Parsing

data Point = P Int Int Int deriving (Eq, Ord)
type Beacons = S.Set Point
type Shift = Point
type Scanners = [Point]
type Input = [Beacons]

instance Num Point where
  (P a b c) + (P d e f) = P (a + d) (b + e) (c + f)
  (P a b c) - (P d e f) = P (a - d) (b - e) (c - f)
  (P a b c) * (P d e f) = P (a * d) (b * e) (c * f)
  abs (P a b c)         = undefined
  signum p              = undefined
  fromInteger           = undefined

parser :: String -> Input
parser =
  map (S.fromList . map f . tail) . splitOn [""] . lines
  where
    f s = let [a,b,c] = map read $ splitOn "," s in P a b c

part1 :: Input -> Int
part1 = length . fst . alignAll

part2 :: Input -> Int
part2 = maximum . allDistances . snd . alignAll
  where
    allDistances ps = [norm $ p1 - p2 | p1 <- ps, p2 <- ps, p1 /= p2]
    norm (P a b c) = abs a + abs b + abs c

allRotations :: S.Set Point -> [S.Set Point]
allRotations ps =
  [ S.map (f . g . h . i) ps
  | f <- [id, flipFirst]
  , g <- [id, flipSecond]
  , h <- [id, flipThird]
  , i <- [swap123, swap132, swap213, swap231, swap312, swap321]
  ]
  where
    flipFirst  (P a b c) = P (-a) b c
    flipSecond (P a b c) = P a (-b) c
    flipThird  (P a b c) = P a b (-c)
    swap123    (P a b c) = P a b c 
    swap132    (P a b c) = P a c b
    swap213    (P a b c) = P b a c
    swap231    (P a b c) = P b c a
    swap312    (P a b c) = P c a b
    swap321    (P a b c) = P c b a

tryToAlign :: Beacons -> Beacons -> Either Beacons (Beacons, Shift)
tryToAlign fixed ps =
  if null shifts
    then Left ps
    else Right $ head shifts
  where
    shifts =
      [ (shifted, shift)
      | fixedPoint <- S.toList fixed
      , rotatedPs <- allRotations ps
      , p <- S.toList rotatedPs
      , let shift = fixedPoint - p
      , let shifted = S.map (+ shift) rotatedPs
      , let intersectionSize = length (fixed `S.intersection` shifted)
      , intersectionSize >= 12
      ]

alignFirst :: (Beacons, Scanners, [Beacons]) -> (Beacons, Scanners, [Beacons])
alignFirst (aligned, shifts, notAligned) = (aligned', shifts', notAligned')
  where
    (notAligned', res) = partitionEithers $ map (tryToAlign aligned) notAligned
    aligned' = S.unions $ aligned : map fst res
    shifts' = shifts ++ map snd res

alignAll :: [Beacons] -> (Beacons, Scanners)
alignAll (ps:pss) = dropThird . head . filter (null . third) $ iterate alignFirst (ps, [], pss)
    where dropThird (x, y, _) = (x, y)
          third     (_, _, x) = x
alignAll [] = undefined

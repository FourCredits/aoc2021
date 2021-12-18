module Day18 where

import Control.Arrow
import Data.Char
import Data.List

data Snailfish
  = Regular Int
  | Pair Snailfish Snailfish
  deriving (Eq)

instance Show Snailfish where
  show (Regular a) = show a
  show (Pair a b) = "[" ++ show a ++ "," ++ show b ++ "]"

parser :: String -> [Snailfish]
parser = map snailfish . lines

snailfish :: String -> Snailfish
snailfish = fst . fish
  where
    fish :: String -> (Snailfish, String)
    fish s@(c:cs)
      | isDigit c = first Regular $ head (reads s :: [(Int, String)])
      | otherwise = pair s
    fish [] = undefined
    pair :: String -> (Snailfish, String)
    pair ('[':s) =
      let (a, ',':s')  = fish s
          (b, ']':s'') = fish s'
       in (Pair a b, s'')
    pair _ = undefined

part1 :: [Snailfish] -> Int
part1 = magnitude . foldl1' add

part2 :: [Snailfish] -> Int
part2 ns =
  maximum $
  map (magnitude . uncurry add) $ filter (uncurry (/=)) $ (,) <$> ns <*> ns

magnitude :: Snailfish -> Int
magnitude (Regular a) = a
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

add :: Snailfish -> Snailfish -> Snailfish
add a b = reduce $ Pair a b

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

reduce :: Snailfish -> Snailfish
reduce n = maybe (maybe n reduce (split n)) reduce (fst3 <$> explode n)

explode :: Snailfish -> Maybe (Snailfish, Int, Int)
explode (Regular a) = undefined
explode n = explode' 0 n
  where
    explode' :: Int -> Snailfish -> Maybe (Snailfish, Int, Int)
    explode' _ (Regular _) = Nothing
    explode' 4 (Pair (Regular a) (Regular b)) = Just (Regular 0, a, b)
    explode' 4 (Pair _ _) = error "unreachable"
    explode' d (Pair a b) =
      case explode' (d + 1) a of
        Just (a', l, r) -> Just (Pair a' (addl r b), l, 0)
        Nothing ->
          case explode' (d + 1) b of
            Just (b', l, r) -> Just (Pair (addr l a) b', 0, r)
            Nothing -> Nothing

split :: Snailfish -> Maybe Snailfish
split (Regular a)
  | a >= 10 = Just $ Pair (Regular $ a `div` 2) (Regular $ (a + 1) `div` 2)
  | otherwise = Nothing
split (Pair a b) =
  case split a of
    Just a' -> Just $ Pair a' b
    Nothing ->
      case split b of
        Just b' -> Just $ Pair a b'
        Nothing -> Nothing

-- Add `n` to the leftmost regular value in the snailfish number
addl :: Int -> Snailfish -> Snailfish
addl a (Regular b) = Regular (a + b)
addl a (Pair b c) = Pair (addl a b) c

-- Adr `n` to the rightmost regular value in the snailfish number
addr :: Int -> Snailfish -> Snailfish
addr a (Regular b) = Regular (a + b)
addr a (Pair b c) = Pair b (addr a c)

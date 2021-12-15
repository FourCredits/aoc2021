{-# LANGUAGE TupleSections #-}

module Utils.Misc where

import Data.Char
import Data.List
import qualified Data.Map.Strict as M

-- Parses a single digit as a number i.e. '9' becomes 9
singleDigit :: Char -> Int
singleDigit c
  | c `elem` ['0' .. '9'] = ord c - ord '0'
  | otherwise             = undefined

-- | Folds a list of a lists into a single list with Data.List.Union
unions :: (Eq a) => [[a]] -> [a]
unions = foldr union []

-- Takes a list of values, and returns a map relating each value in the list to
-- the number of times it appeared in the list
counter :: (Ord a) => [a] -> M.Map a Int
counter coll = M.fromListWith (+) $ map (, 1) coll

simulateN :: Int -> (a -> a) -> a -> a
simulateN n tick start = iterate tick start !! n

-- Returns the power set of the input list: each possible subset of the input
-- list.
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = map (x :) ps ++ ps
  where
    ps = powerSet xs

-- Returns the power set of the input list, minus the empty list and the input
-- list itself. Results are sorted on size.
powerSet' :: [a] -> [[a]]
powerSet' xs =
  sortOn length $
  filter (\set -> not (null set) && length set /= l) $ powerSet xs
  where
    l = length xs

module Day08 where

import Control.Arrow
import Control.Monad
import Data.Maybe
import Data.Function
import Data.List

type Pattern = String

type Candidate = (Char, String)

parser :: String -> [([Pattern], [Pattern])]
parser = map parseLine . lines
  where
    parseLine = (words *** (tail . words)) . break (== '|')

part1 :: [([Pattern], [Pattern])] -> Int
part1 ls = sum $ map (countUniques . snd) ls
  where
    countUniques = length . filter ((`elem` [2, 3, 4, 7]) . length)

part2 :: [([Pattern], [Pattern])] -> Int
part2 entries = sum $ map calculateOutputValue entries
  where
    calculateOutputValue (patterns, outputValue) =
      readSevenSeg $ correct (createMapping patterns) outputValue

correct :: [(Char, Char)] -> [Pattern] -> [Pattern]
correct mapping = map (map (lookup' mapping))

readSevenSeg :: [Pattern] -> Int
readSevenSeg =
  foldl' (\acc n -> (acc * 10) + n) 0 . map (lookup' sevenSegTable . sort)

sevenSegPatterns :: [Pattern]
sevenSegPatterns =
  [ "abcefg"
  , "cf"
  , "acdeg"
  , "acdfg"
  , "bcdf"
  , "abdfg"
  , "abdefg"
  , "acf"
  , "abcdefg"
  , "abcdfg"
  ]

sevenSegTable :: [(Pattern, Int)]
sevenSegTable = zip sevenSegPatterns [0 ..]

-- Given a list of patterns, creates a mapping relating each displayed segment
-- to its correct one.
createMapping :: [Pattern] -> [(Char, Char)]
createMapping patterns = go start
  where
    start              = zip ['a' .. 'g'] (replicate 7 ['a' .. 'g'])
    matchPL candidates = foldr matchPatternLength   candidates patterns
    matchPS candidates = foldr matchPatternSegments candidates patterns
    done               = all ((== 1) . length . snd)
    techniques         = cycle [matchPL , nakedSubsets , matchPS]
    go candidates      = map (second head) $ chain done techniques candidates

-- Finds any naked subsets in the input list, and filters those values from the
-- other candidates.
--
-- A subset is naked if it contains a number of possibilities equal to the size
-- of the subset. For example, in `[('d', "acf"), ('a', "cf"), ('b', "cf")]`,
-- 'a' and 'b' form a naked subset. Because both of them can be *must* be one of
-- 'c' or 'f', 'd' cannot be either of those options, and so you can remove "cf"
-- from its set of possibilities.
nakedSubsets :: [Candidate] -> [Candidate]
nakedSubsets patterns =
  case filter (((/=) `on` sort) patterns) newPatterns of
    []            -> patterns
    (patterns':_) -> nakedSubsets patterns'
  where
    newPatterns = do
      subset <- powerSet' patterns
      let notesOfSubset = unions $ map snd subset
      -- is it a naked subset?
      guard $ length subset == length notesOfSubset
      let notInSubset = patterns \\ subset
          updatedCandidates = map (second (\\ notesOfSubset)) notInSubset
      return $ update patterns updatedCandidates

-- Based on the given pattern's length, determines what numbers it could be
-- referring to, and updates the segments so to reflect this constraint.
matchPatternLength :: Pattern -> [Candidate] -> [Candidate]
matchPatternLength pat candidates = do
  let possibleLetters    = unions $ fitPattern pat
      candidatesToUpdate = filter ((`elem` pat) . fst) candidates
      updatedCandidates  =
        map (second (intersect possibleLetters)) candidatesToUpdate
   in update candidates updatedCandidates

-- Finds the digits that a given pattern could be referring to, based on the
-- number of segments needed to represent them.
fitPattern :: Pattern -> [Pattern]
fitPattern pat = filter ((== length pat) . length) $ map fst sevenSegTable

-- Based on the given pattern, determines what number(s) it could be referring
-- to. The segments in the pattern can then be restricted to only be the
-- segments used to make that number(s).
--
-- As an example, if the given pattern could only refer to 5, which has the
-- pattern abdfg, then the notes in the given pattern can be restricted to only
-- map to a, b, d, f, or g.
matchPatternSegments :: Pattern -> [Candidate] -> [Candidate]
matchPatternSegments pat candidates =
  let relevantCandidates = filter ((`elem` pat) . fst) candidates
      possibleDigits     =
        filter (`elem` map fst sevenSegTable) $
        map sort $ possibilities $ map snd relevantCandidates
      notesToKeep        = unions possibleDigits
      updatedCandidates  =
        map (second (intersect notesToKeep)) relevantCandidates
   in update candidates updatedCandidates

-- utility functions

-- `chain` is a function for applying different transformations to a value. It
-- takes a predicate function, a list of transformations, and a base value. It
-- applies the transformations in succession to the base value until the
-- predicate returns true.
chain :: (a -> Bool) -> [a -> a] -> a -> a
chain stop = go
  where
    go [] a = a
    go (t:ts) a
      | stop a = a
      | otherwise = go ts (t a)

isSubsetOf :: (Eq a) => [a] -> [a] -> Bool
isSubsetOf as bs = all (`elem` bs) as

-- `possibilities` takes a list of lists. Each inner list is treated as a
-- non-deterministic value. The function returns each possible value.
-- e.g. `possibilities [[1, 2], [3, 4]]` returns `[[1, 3], [1, 4], [2, 3], [2, 4]]`
possibilities :: [[a]] -> [[a]]
possibilities = foldr (\x acc -> (:) <$> x <*> acc) [[]]

-- A right fold over `Data.List.union`.
unions :: (Eq a) => [[a]] -> [a]
unions = foldr union []

-- Updates the first assoc list with values from the second. The ordering is not
-- preserved.
update :: (Eq a) => [(a, b)] -> [(a, b)] -> [(a, b)]
update original updates = unionBy ((==) `on` fst) updates original

lookup' :: (Eq a) => [(a, b)] -> a -> b
lookup' table k = fromJust $ lookup k table

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

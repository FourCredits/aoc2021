module Day10 where

import Data.List
import Debug.Trace

part1 :: [String] -> Integer
part1 = sum . map scoreOfLine

part2 :: [String] -> Integer
part2 = median . map getScore . filter (not . null) . map closingChars

median :: (Ord a) => [a] -> a
median xs = sort xs !! m
  where m = length xs `div` 2

getScore :: String -> Integer
getScore = foldl' total 0
  where
    total acc c = (5 * acc) + score c
    score ')' = 1
    score ']' = 2
    score '}' = 3
    score '>' = 4
    score _ = undefined

closingChars :: String -> String
closingChars = go []
  where
    go stack [] = map inverse stack
    go stack (b:bs)
      | b `elem` "([{<" = go (b:stack) bs
    go ('(':as) (')':bs) = go as bs
    go ('[':as) (']':bs) = go as bs
    go ('{':as) ('}':bs) = go as bs
    go ('<':as) ('>':bs) = go as bs
    go stack (b:bs)
      | b `elem` ")]}>" = []
    go _ _ = undefined

inverse :: Char -> Char
inverse '(' = ')'
inverse '[' = ']'
inverse '{' = '}'
inverse '<' = '>'
inverse _ = undefined

scoreOfLine :: String -> Integer
scoreOfLine = go []
  where
    go stack [] = 0
    go stack (b:bs)
      | b `elem` "([{<" = go (b:stack) bs
    go ('(':as) (')':bs) = go as bs
    go ('[':as) (']':bs) = go as bs
    go ('{':as) ('}':bs) = go as bs
    go ('<':as) ('>':bs) = go as bs
    go stack (b:bs)
      | b `elem` ")]}>" =
        case b of
          ')' -> 3
          ']' -> 57
          '}' -> 1197
          '>' -> 25137
          _   -> undefined
    go _ _ = undefined
          

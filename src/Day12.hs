module Day12 where

import Data.Char
import Debug.Trace

import Utils

type Input = [(String, String)]

parser :: String -> Input
parser = concatMap f . lines
  where
    f s =
      let (a, '-':b) = break (== '-') s
       in [(a, b), (b, a)]

part1 :: Input -> Int
part1 input = length $ findEnd p input ["start"]
  where
    p [] _ = undefined
    p path@(c:_) (from, to) =
      from == c && (isBigCave to || to `notElem` path)

part2 :: Input -> Int
part2 input = length $ findEnd p input ["start"]
  where
    p [] _ = undefined
    p path@(c:_) (from, to) =
      from == c &&
      to /= "start" && (isBigCave to || notInPath || noSmallCaveVisistedTwice)
      where
        notInPath = to `notElem` path
        noSmallCaveVisistedTwice =
          all (== 1) $ counter $ filter isSmallCave path

-- TODO: create helper function for the 'input' variable

findEnd ::
     ([String] -> (String, String) -> Bool) -> Input -> [String] -> [[String]]
-- findEnd p input path@("end":_) = [path]
-- findEnd p input path@(c:cs) =
--   concatMap (findEnd p input) $ nextSteps p input path
-- findEnd _ _ [] = undefined
findEnd p input = go
  where
    go [] = undefined
    go path@("end":_) = [path]
    go path = concatMap go $ nextSteps p input path

nextSteps ::
     ([String] -> (String, String) -> Bool) -> Input -> [String] -> [[String]]
nextSteps p input [] = undefined
nextSteps p input path@(c:_) =
  map ((: path) . snd) $ filter (p path) input

isBigCave :: String -> Bool
isBigCave []    = undefined
isBigCave (x:_) = isUpper x

isSmallCave :: String -> Bool
isSmallCave = not . isBigCave

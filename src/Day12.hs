module Day12 where

import Data.Char
import Data.Function
import Data.Graph
import Data.List

import Utils

type Input
   = (Graph, Vertex -> (String, String, [String]), String -> Maybe Vertex)
type Path = [String]
type Predicate = Path -> String -> Bool

parser :: String -> Input
parser =
  graphFromEdges . map g . groupBy ((==) `on` fst) . sort . concatMap f . lines
  where
    f s =
      let (a, '-':b) = break (== '-') s
       in [(a, b), (b, a)]
    g edges@((vertex, _):_) = (vertex, vertex, map snd edges)
    g [] = undefined

part1, part2 :: Input -> Int
part1 = solve p1
part2 = solve p2

solve :: Predicate -> Input -> Int
solve p input = length $ go ["start"]
  where
    go [] = undefined
    go path@("end":_) = [path]
    go path = concatMap go $ nextSteps p path input

nextSteps :: Predicate -> Path -> Input -> [Path]
nextSteps _ [] _ = []
nextSteps p path@(c:_) (graph, nfv, vfk) =
  map (: path) $ filter (p path) $ trd3 $ nfv v
  where
    Just v = vfk c
    trd3 (_, _, c) = c

p1, p2 :: Predicate

p1 path to = isBigCave to || to `notElem` path

p2 path to =
  isBigCave to ||
  to `notElem` path ||
  (to /= "start" && all (== 1) (counter $ filter (not . isBigCave) path))

isBigCave :: String -> Bool
isBigCave = isUpper . head

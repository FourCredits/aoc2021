module Day20 where

import Control.Arrow
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
-- import qualified Data.Set as S
import Data.Array.IArray (range)

import Utils.Parsing
import Utils.TwoD
import Utils.Misc

type Image     = Position -> Bool
type Algorithm = [Bool]
type Input     = (Algorithm, Image, (Int, Int))

-- TODO: make dims function abstracting the length and length head thing?

parser :: String -> Input
parser s =
  let isLight              = (== '#')
      (algorithmS, imageS) = twoOf $ splitOn "\n\n" s
      algorithm            = map isLight algorithmS
      ls                   = lines imageS
      h                    = length s
      w                    = length (head ls)
      r                    = range ((0, 0), (h - 1, w - 1))
      m                    = M.fromList $ zip r (map isLight (concat ls))
      image pos            = M.findWithDefault False pos m
   in (algorithm, image, (h, w))

square :: Position -> [Position]
square (x, y) = [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1]]

enhanceN :: Int -> Algorithm -> Image -> Image
enhanceN n algo image = iterate (enhance algo) image !! n

enhance :: Algorithm -> Image -> Image
enhance algo image pos = algo !! bitsToInt (map image $ square pos)

pixelToChar :: Bool -> Char
pixelToChar  True = '#'
pixelToChar False = '.'

part1 :: Input -> Int
part1 (algo, image, (h, w)) =
  let image' = enhanceN 2 algo image
      r = range ((-4, -4), (h + 4, w + 4))
   in length $ filter id $ map image' r

-- TODO: move to utils
chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

showRange :: (Position, Position) -> Image -> IO ()
showRange (lo, hi) image =
  putStrLn $
  unlines $
  chunksOf (1 + (fst hi - fst lo)) $ map (pixelToChar . image) (range (lo, hi))

{-
index :: Image -> Position -> Int
index image pos =
  bitsToInt $ map (\p -> M.findWithDefault False p image) $ square pos

enhance :: Algorithm -> Image -> Image
enhance algo image = M.mapWithKey (\k _ -> algo !! index image k) image

showImage :: Image -> IO ()
showImage image =
  putStrLn $
  unlines $
  map (map ((\c -> if c then '#' else '.') .  snd)) $
  groupBy ((==) `on` (fst . fst)) $ M.assocs image
-}

-- TODO: remove
example =
  parser $
  unlines
    [ concat
        [ "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##"
        , "#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###"
        , ".######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#."
        , ".#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#....."
        , ".#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.."
        , "...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#....."
        , "..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
        ]
    , ""
    , "#..#."
    , "#...."
    , "##..#"
    , "..#.."
    , "..###"
    ]

(algo, image, (h,w)) = example

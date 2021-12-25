module Day20 where

import Data.Array.IArray
import Data.Function
import Data.List

import Utils.Misc
import Utils.Parsing
import Utils.TwoD

newtype Image  = Image { getImage :: Array Position Bool }
type Rectangle = (Position, Position) -- TODO: move to Utils.TwoD
type Algorithm = [Bool]
type Input     = (Algorithm, Image)

parser :: String -> Input
parser s = (algorithm, image)
  where
    algorithm            = map isLight algorithmS
    image                = Image $ listArray rectangle (map isLight (concat ls))
    rectangle            = ((1, 1), dims ls)
    (algorithmS, imageS) = twoOf $ splitOn "\n\n" s
    ls                   = lines imageS
    isLight              = (== '#')

-- part 1 is 4968
part1 :: Input -> Int
part1 = countLit . enhance 2

-- part 2 is 16793
part2 :: Input -> Int
part2 = countLit . enhance 50

countLit :: Image -> Int
countLit (Image i) = length $ filter id $ elems i

enhance :: Int -> Input -> Image
enhance n (algorithm, image) = image''
  where
    image' = expandImage n image
    image'' = fst $ simulateN n enhance' (image', False)
    bs = bounds (getImage image')
    positions = range bs
    enhance' (Image i, def) = (Image i', def')
      where
        i' =
          listArray bs $
          map
            (\pos -> algorithm !! bitsToInt (map (i !?) $ square pos))
            positions
        def' = algorithm !! bitsToInt (replicate 9 def)
        a !? p = if inRange bs p then a ! p else def

expandImage :: Int -> Image -> Image
expandImage n (Image image) =
  Image $
  listArray bs' $ do
    pos <- range bs'
    return (inRange bs pos && image ! pos)
  where
    bs = bounds image
    bs' = expandRange n bs

expandRange :: Int -> (Position, Position) -> (Position, Position)
expandRange n ((yLo, xLo), (yHi, xHi)) =
  ((yLo - n, xLo - n), (yHi + n, xHi + n))

-- TODO: move to Utils.TwoD
dims :: [[a]] -> (Int, Int)
dims as = (length as, length (head as))

square :: Position -> [Position]
square (x, y) = [(x', y') | x' <- [x - 1 .. x + 1], y' <- [y - 1 .. y + 1]]

module Day24 where

import Control.Monad.State
import Data.List
import qualified Data.Set as S

import Utils.Misc

data Val = Lit Int | Var Char deriving (Eq)
type ALU = (Int, Int, Int, Int)
type Mach = State ([Int], ALU)

--           addX divZ addY
type Step = (Int, Int, Int)
type Input = [Step]

-- part 2 is 48111514719111
part1, part2 :: Input -> Int
part1 steps = undigits $ head $ evalState (solve [9, 8 .. 1] 0 steps) S.empty
part2 steps = undigits $ head $ evalState (solve [1 .. 9]    0 steps) S.empty

parser :: String -> Input
parser = map parseStep . chunksOf 18 . map words . lines
  where
    parseStep input =
      (read (input !! 5 !! 2), read (input !! 4 !! 2), read (input !! 15 !! 2))

step :: Int -> Step -> Int -> Int
step z (addX, divZ, addY) w =
  let x  = ((z `rem` 26) + addX)
      z' = z `quot` divZ
  in  if x /= w then (z' * 26) + (w + addY) else z'

solve :: [Int] -> Int -> [Step] -> State (S.Set (Int, Int)) [[Int]]
solve ws z (s : steps) = do
  dp <- get
  if S.member (z, length steps) dp
    then pure []
    else do
      result <-
        concat
          <$> traverse
                (\w ->
                  let z' = step z s w
                  in  if null steps
                        then (if z' == 0 then pure [[w]] else pure [])
                        else map (w :) <$> solve ws (step z s w) steps
                )
                ws
      modify (S.insert (z, length steps))
      pure result
solve _ _ [] = undefined

undigits :: [Int] -> Int
undigits = foldl' (\acc n -> (acc * 10) + n) 0

digits :: Int -> [Int]
digits = reverse . go
  where go n = if n < 10 then [n] else (n `mod` 10) : go (n `div` 10)

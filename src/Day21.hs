module Day21 where

import Control.Monad.State
import Data.Foldable
import qualified Data.Map.Strict as M
import Data.Semigroup
import Debug.Trace

import Utils.Misc
import Utils.Parsing

type Input = (Int,Int)
type Position = Int
type Score = Int
data PlayerIndicator = One | Two deriving (Show, Eq, Ord)
data Player = P Position Score deriving (Show, Eq, Ord)

parser :: String -> Input
parser = twoOf . map (read . last . words) . lines

takeTurn :: Int -> Player -> (Int, Player)
takeTurn die (P pos score) = (die + 3, P pos' (score + pos'))
  where
    pos' = (pos + (3 * (die `wrap` 100) + 3)) `wrap` 10

playDeterministic :: Player -> Player -> (Int, Player, Player)
playDeterministic = play One 1
  where
    play turn die p1 p2
      | hasWon p1 = (die - 1, p1, p2)
      | hasWon p2 = (die - 1, p2, p1)
      | otherwise =
        case turn of
          One -> let (die', p1') = takeTurn die p1 in play Two die' p1' p2
          Two -> let (die', p2') = takeTurn die p2 in play One die' p1  p2'

hasWon :: Player -> Bool
hasWon (P _ score) = score >= 1000

part1 :: Input -> Int
part1 (pos1, pos2) =
  let player1 = P pos1 0
      player2 = P pos2 0
      (rolls, win, loss@(P _ losingScore)) = playDeterministic player1 player2
   in rolls * losingScore

type Memo = M.Map (Player, Player) (Sum Int, Sum Int)

swap :: (a, b) -> (b, a)
swap (a,b) = (b,a)

-- TODO: refactor memoize function
playQuantum :: Player -> Player -> State Memo (Sum Int, Sum Int)
playQuantum p1@(P _ s1) p2@(P _ s2)
  | s1 >= 21 = return (1, 0)
  | s2 >= 21 = return (0, 1)
playQuantum p1@(P pos score) p2 = do
  memo <- get
  case memo M.!? (p1, p2) of
    Just result -> return result
    Nothing -> do
      let players' =
            [ P pos' (score + pos')
            | d1 <- [1, 2, 3]
            , d2 <- [1, 2, 3]
            , d3 <- [1, 2, 3]
            , let roll = d1 + d2 + d3
            , let pos' = (pos + roll) `wrap` 10
            ]
      subUniverses <- traverse (playQuantum p2) players'
      let result = swap $ fold subUniverses
      modify (M.insert (p1, p2) result)
      return result

-- part2 :: Input -> Int
part2 (pos1, pos2) = getSum $ uncurry max $ evalState (playQuantum (P pos1 0) (P pos2 0)) M.empty

-- testData :: Input
-- testData = (4,8)

-- parseEx :: String
-- parseEx =
--   "Player 1 starting position: 4\n\
--   \Player 2 starting position: 8"

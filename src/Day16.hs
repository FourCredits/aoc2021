module Day16 where

import Control.Arrow
import Data.Bits
import Data.List
import qualified Data.Map.Strict as M
import Debug.Trace

type Parser a = [Bool] -> (a, [Bool])
type Version = Int
type Type = Int
data Packet
  = Literal Version Int
  | Operator Version Type [Packet]
  deriving (Show, Eq, Ord)

parser :: String -> [Bool]
parser = concatMap (hexToBinary M.!)
  where
    hexToBinary =
      M.fromList
        [ ('0', [False, False, False, False])
        , ('1', [False, False, False,  True])
        , ('2', [False, False,  True, False])
        , ('3', [False, False,  True,  True])
        , ('4', [False,  True, False, False])
        , ('5', [False,  True, False,  True])
        , ('6', [False,  True,  True, False])
        , ('7', [False,  True,  True,  True])
        , ('8', [ True, False, False, False])
        , ('9', [ True, False, False,  True])
        , ('A', [ True, False,  True, False])
        , ('B', [ True, False,  True,  True])
        , ('C', [ True,  True, False, False])
        , ('D', [ True,  True, False,  True])
        , ('E', [ True,  True,  True, False])
        , ('F', [ True,  True,  True,  True])
        ]

part1 :: [Bool] -> Int
part1 = sumVersions . fst . bitsParser

sumVersions :: Packet -> Int
sumVersions (Literal v _) = v
sumVersions (Operator v _ sub) = v + sum (map sumVersions sub)

part2 :: [Bool] -> Int
part2 = evalPacket . fst . bitsParser

evalPacket :: Packet -> Int
evalPacket (Literal _ i)     = i
evalPacket (Operator _ 0 ps) = sum $ map evalPacket ps
evalPacket (Operator _ 1 ps) = product $ map evalPacket ps
evalPacket (Operator _ 2 ps) = minimum $ map evalPacket ps
evalPacket (Operator _ 3 ps) = maximum $ map evalPacket ps
evalPacket (Operator _ 5 ps) =
  let [a, b] = map evalPacket ps in if a > b then 1 else 0
evalPacket (Operator _ 6 ps) =
  let [a, b] = map evalPacket ps in if a < b then 1 else 0
evalPacket (Operator _ 7 ps) =
  let [a, b] = map evalPacket ps in if a == b then 1 else 0
evalPacket _ = error "unknown type id"

bitsToInt :: [Bool] -> Int
bitsToInt = foldl' shift 0
  where
    shift n True  = shiftL n 1 .|. 1
    shift n False = shiftL n 1 .|. 0

-------------
-- parsing --
-------------

bitsParser :: Parser Packet
bitsParser p =
  let (v, p') = version p
      (t, p'') = typeId p'
   in if isLiteral t
        then first (Literal v) $ literal p''
        else first (Operator v t) $ operator p''

literal :: Parser Int
literal = first bitsToInt . go
  where
    go packet = 
      case splitAt 5 packet of
        ( True:bits, rest) -> first (bits ++) $ go rest
        (False:bits, rest) -> (bits, rest)
        _                  -> undefined

operator :: Parser [Packet]
operator (False:p') = totalLength p'
operator ( True:p') = numberSubpackets p'
operator _          = undefined

totalLength :: Parser [Packet]
totalLength = uncurry (consumeBy parseAll) . consumeBy bitsToInt 15
  where
    parseAll = unfoldr (\b -> if null b then Nothing else Just $ bitsParser b)

numberSubpackets :: Parser [Packet]
numberSubpackets = uncurry parseN . consumeBy bitsToInt 11
  where
    parseN 0 bits = ([], bits)
    parseN n bits =
      let (p, bits') = bitsParser bits in first (p :) $ parseN (n - 1) bits'

version :: Parser Version
version = consumeBy bitsToInt 3

typeId :: Parser Type
typeId = consumeBy bitsToInt 3

isLiteral :: Int -> Bool
isLiteral 4 = True
isLiteral _ = False

consumeBy :: ([Bool] -> a) -> Int -> Parser a
consumeBy f n = first f . splitAt n

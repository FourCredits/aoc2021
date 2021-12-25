{-# LANGUAGE ViewPatterns #-}

module Day24 where

import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map.Strict as M
import Debug.Trace -- TODO: Remove
import Text.Read (readMaybe)

data Variable = W | X | Y | Z deriving (Show, Eq, Ord)
data Operation = Add | Mul | Div | Mod | Eql deriving (Show, Eq, Ord)
data Instruction
  = Inp Variable
  | Op Operation Variable (Either Variable Int)
  deriving (Show, Eq, Ord)
type Program = [Instruction]
type Input = [Int]
type Computer = M.Map Variable Int

parser :: String -> Program
parser = map instruction . lines
  where
    instruction (words -> ["inp",a   ]) = Inp (variable a)
    instruction (words -> ["add",a, b]) = Op Add (variable a) (address b)
    instruction (words -> ["mul",a, b]) = Op Mul (variable a) (address b)
    instruction (words -> ["div",a, b]) = Op Div (variable a) (address b)
    instruction (words -> ["mod",a, b]) = Op Mod (variable a) (address b)
    instruction (words -> ["eql",a, b]) = Op Eql (variable a) (address b)
    instruction _                       = error "unrecognized instruction"
    address :: String -> Either Variable Int
    address s
      | Just a <- readMaybe s = Right a
      | otherwise = Left $ variable s
    variable "w" = W
    variable "x" = X
    variable "y" = Y
    variable "z" = Z
    variable _   = undefined

startComp :: Computer
startComp = M.fromList [(W, 0), (X, 0), (Y, 0), (Z, 0)]

part1 :: Program -> Int
part1 program =
  maximum [undigits ns | ns <- replicateM 14 [1 .. 9], monad ns program]

monad :: [Int] -> Program -> Bool
monad ns program
  | length ns /= 14 = error "model number must be 14 digits"
  | 0 `elem` ns = error "can't have 0s in a model number"
  | otherwise = 0 == (runComputer ns program startComp M.! Z)

runComputer :: Input -> Program -> Computer -> Computer
runComputer _      [] comp = comp
runComputer (n:ns) (Inp a:is) comp = runComputer ns is (M.insert a n comp)
runComputer []     (Inp _      :is) comp = error "no more input"
runComputer ns (Op Add a b:is) comp = runComputer ns is (op (+) a b comp)
runComputer ns (Op Mul a b:is) comp = runComputer ns is (op (*) a b comp)
runComputer ns (Op Div a b:is) comp = runComputer ns is (op div a b comp)
runComputer ns (Op Mod a b:is) comp = runComputer ns is (op mod a b comp)
runComputer ns (Op Eql a b:is) comp = runComputer ns is (op eql a b comp)

op ::
     (Int -> Int -> Int)
  -> Variable
  -> Either Variable Int
  -> Computer
  -> Computer
op f a b comp = M.insertWith (flip f) a (getValue comp b) comp

getValue :: Computer -> Either Variable Int -> Int
getValue comp (Left a) = comp M.! a
getValue _    (Right a) = a

eql :: Int -> Int -> Int
eql a b = if a == b then 1 else 0

undigits :: [Int] -> Int
undigits = foldl' (\acc n -> (acc * 10) + n) 0

digits :: Int -> [Int]
digits = reverse . go
  where go n = if n < 10 then [n] else (n `mod` 10) : go (n `div` 10)

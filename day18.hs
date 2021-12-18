-- Advent of Code 2021, day 18

module Main where

import System.Environment
import Data.List
import Data.Char

import Control.Applicative
import qualified Data.Map as M

import FunParser
import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
  -- putStrLn (show $ foldl1 addSN xs)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

data SNum = Reg Int | Pair SNum SNum
  deriving (Eq,Show)

showSN :: SNum -> String
showSN (Reg x) = show x
showSN (Pair x y) = "[" ++ showSN x ++ "," ++ showSN y ++ "]"

data Action = Explode (Maybe Int) (Maybe Int) | Split | NoA
  deriving (Eq,Show)

pSNum :: Parser SNum
pSNum = (natural >>= return.Reg) <|>
        delim "["
               (do x <- pSNum
                   symbol ","
                   y <- pSNum
                   return (Pair x y))
               "]"
               
pInput :: Parser [SNum]
pInput = pLines pSNum

-- Part 1

reduce :: SNum -> SNum
reduce x = let (a,x') = reduceStep 0 x in
  case a of
    NoA -> x'
    _   -> reduce x'

redExplode :: Int -> SNum -> (Action,SNum)
redExplode 4 (Pair (Reg x) (Reg y)) = (Explode (Just x) (Just y), (Reg 0))
redExplode n (Pair x y) =
  let (ax,x') = redExplode (n+1) x in
    case ax of
      Explode u (Just v) -> (Explode u Nothing, Pair x' (addL v y))
      Explode u Nothing -> (Explode u Nothing, Pair x' y)
      NoA -> let (ay,y') = redExplode (n+1) y in
               case ay of
                 Explode (Just u) v -> (Explode Nothing v, Pair (addR u x') y')
                 Explode Nothing v -> (Explode Nothing v, Pair x' y')
                 NoA -> (NoA, Pair x' y')
redExplode n x = (NoA,x)

redSplit :: Int -> SNum -> (Action,SNum)
redSplit n (Reg x) | x>=10 = (Split, split x)
redSplit n (Pair x y) =
  let (ax,x') = redSplit (n+1) x in
    case ax of
      Split -> (Split, Pair x' y)
      NoA -> let (ay,y') = redSplit (n+1) y in
               case ay of
                 Split -> (Split, Pair x' y')
                 NoA -> (NoA, Pair x' y')
redSplit n x = (NoA,x)

reduceStep :: Int -> SNum -> (Action,SNum)
reduceStep n x =
  let (a,x') = redExplode n x in
    case a of
      Explode _ _ -> (a,x')
      NoA -> redSplit n x

addL :: Int -> SNum -> SNum
addL v (Reg x) = Reg (v+x)
addL v (Pair x y) = Pair (addL v x) y

addR :: Int -> SNum -> SNum
addR u (Reg x) = Reg (u+x)
addR u (Pair x y) = Pair x (addR u y)

split :: Int -> SNum
split x = Pair (Reg (x `div` 2)) (Reg (x `div` 2 + x `mod` 2))


addSN :: SNum -> SNum -> SNum
addSN x y = reduce (Pair x y)

magnitude :: SNum -> Int
magnitude (Reg x) = x
magnitude (Pair x y) = 3*(magnitude x) + 2*(magnitude y)

part1 :: [SNum] -> Int
part1 = magnitude . foldl1 addSN

-- Part 2

part2 :: [SNum] -> Int
part2 _ = 2

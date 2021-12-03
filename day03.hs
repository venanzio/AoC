-- Advent of Code 2021, day 3

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pInput :: Parser [[Int]]
pInput = pLines digits  -- someP binLine

-- Part 1

countPos :: [[Int]] -> Int -> Int -> Int
countPos xs y i = length (filter (==y) (map (!!i) xs))

gamma :: [[Int]] -> [Int]
gamma xs =  map (\i -> if countPos xs 1 i >= countPos xs 0 i then 1 else 0) [0..11]

epsilon :: [[Int]] -> [Int]
epsilon xs =  map (\i -> if countPos xs 1 i >= countPos xs 0 i then 0 else 1) [0..11]

dec :: [Int] -> Int
dec []  = 0
dec xs = foldl (\y x -> 2*y +x) 0  xs

part1 :: [[Int]] -> Int
part1 xs = dec (gamma xs) * dec (epsilon xs)

-- Part 2

mostCommon :: [[Int]] -> Int -> Int
mostCommon xs i = if (countPos xs 1 i >= countPos xs 0 i) then 1 else 0

leastCommon :: [[Int]] -> Int -> Int
leastCommon xs i = if (countPos xs 1 i < countPos xs 0 i) then 1 else 0

eqAt :: [Int] -> Int -> Int -> Bool
eqAt xs y i = xs!!i == y

mostCrit :: [[Int]] -> Int -> [Int] -> Bool
mostCrit xs i z = eqAt z (mostCommon xs i) i

leastCrit :: [[Int]] -> Int -> [Int] -> Bool
leastCrit xs i z = eqAt z (leastCommon xs i) i

oxigen :: [[Int]] -> [Int]
oxigen = oxAux 0 where
   oxAux i [x] = x
   oxAux i xs  = oxAux (i+1) (filter (mostCrit xs i) xs)

co2 :: [[Int]] -> [Int]
co2 = co2Aux 0 where
   co2Aux i [x] = x
   co2Aux i xs  = co2Aux (i+1) (filter (leastCrit xs i) xs)


part2 :: [[Int]] -> Int
part2 xs = dec (oxigen xs) * dec (co2 xs)

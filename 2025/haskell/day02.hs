-- Advent of Code 2025, day 2
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List

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
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser (Int,Int)
pData = do x <- natural
           symbol "-"
           y <- natural
           return (x,y)

pInput :: Parser [(Int,Int)]
pInput = someSepStr pData ","

-- Solutions (Parts 1 and 2 unified)

-- take the first m digits of x
takeDigs :: Int -> Int -> Int
takeDigs m x = x `div` 10^(dig10 x - m)

-- iterate given digits k times
itDigs :: Int -> Int -> Int
itDigs 0 x = 0
itDigs k x = (itDigs (k-1) x) * 10^(dig10 x) + x

-- minimum k-repeating number larger than x
firstRep :: Int -> Int -> Int
firstRep k x = if itDigs k y < x then y+1 else y
  where  n = dig10 x
         m = n `div` k
         y = if divisible n k then takeDigs m x else 10^m

-- maximum k-repeting number smaller than x
lastRep :: Int -> Int -> Int
lastRep k x = if itDigs k y > x then y-1 else y
  where  n = dig10 x
         m = n `div` k
         y = if divisible n k then takeDigs m x else 10^m-1

-- all k-repetitions in a range
invalidRep :: Int -> Int -> Int -> [Int]
invalidRep k x y = [itDigs k z | z <- [firstRep k x .. lastRep k y]]

-- all repetition for any k
invalidAll :: Int -> Int -> [Int]
invalidAll x y = nub $ concat [invalidRep k x y | k <- [2 .. dig10 y]]

part1 :: [(Int,Int)] -> Int
part1 xys = sum $ concat [invalidRep 2 x y | (x,y) <- xys]

part2 :: [(Int,Int)] -> Int
part2 xys = sum $ concat [invalidAll x y | (x,y) <- xys]

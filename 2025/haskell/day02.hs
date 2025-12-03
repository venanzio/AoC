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

-- Part 1

-- take the first n digits of x

takeDigs :: Int -> Int -> Int
takeDigs n x = x `div` 10^(dig10 x - n)

-- iterate given digits k times
itDigs :: Int -> Int -> Int
itDigs 0 x = 1
itDigs 1 x = x
itDigs k x = (itDigs (k-1) x) * 10^(dig10 x) + x

-- minimum duplicate number larger than x
firstDupl :: Int -> Int
firstDupl x = if itDigs 2 y < x then y+1 else y
  where  n = dig10 x
         m = n `div` 2
         y = if even n then takeDigs m x else 10^m

-- maximum duplicate number smaller than x
lastDupl :: Int -> Int
lastDupl x = if itDigs 2 y > x then y-1 else y
  where  n = dig10 x
         m = n `div` 2
         y = if even n then takeDigs m x else 10^m-1

-- all invalid IDs in a range
invalid :: Int -> Int -> [Int]
invalid x y = [itDigs 2 z | z <- [firstDupl x .. lastDupl y]]

----

part1 :: [(Int,Int)] -> Int
part1 xys = sum $ concat [invalid x y | (x,y) <- xys]

-- Part 2

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

part2 :: [(Int,Int)] -> Int
part2 xys = sum $ concat [invalidAll x y | (x,y) <- xys]

-- Advent of Code 2021, day 6

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

pInput :: Parser [Int]
pInput = seqSep natural ","

-- Part 1

nextDay :: [Int] -> [Int]
nextDay [] = []
nextDay (x:xs) = if x == 0 then 6:8:nextDay xs
                           else (x-1):nextDay xs

nextN :: [Int] -> Int -> [Int]
nextN xs 0 = xs
nextN xs n = nextN (nextDay xs) (n-1)

part1 :: [Int] -> Int
part1 xs = length (nextN xs 80)

-- Part 2

nDay :: [Int] -> [Int]
nDay ls = [if i == 6 then ls!!0 + ls!!7
            else if i == 8 then ls!!0 else ls!!(i+1) | i <- [0..8]]

next :: [Int] -> Int -> [Int]
next ls 0 = ls
next ls n = next (nDay ls) (n-1)

start :: [Int] -> [Int]
start xs = [count i | i <- [0..8]]
  where count i = length (filter (==i) xs)

part2 :: [Int] -> Int
part2 xs = sum (next (start xs) 256)

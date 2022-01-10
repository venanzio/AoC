-- Advent of Code 2021, day 6

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
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pInput :: Parser [Int]
pInput = seqSep natural ","

-- Part 1

nextDay :: [Int] -> [Int]
nextDay ls = [if i == 6 then ls!!0 + ls!!7
              else if i == 8 then ls!!0 else ls!!(i+1) | i <- [0..8]]

afterDays :: [Int] -> Int -> [Int]
afterDays ls 0 = ls
afterDays ls n = afterDays (nextDay ls) (n-1)

start :: [Int] -> [Int]
start xs = [count i | i <- [0..8]]
  where count i = length (filter (==i) xs)

part1 :: [Int] -> Int
part1 = sum . nIter nextDay 80 . start

-- Part 2

part2 :: [Int] -> Int
part2 = sum . nIter nextDay 256 . start

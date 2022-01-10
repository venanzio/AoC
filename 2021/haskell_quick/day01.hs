-- Advent of Code 2021, day 1

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
pInput = some natural

-- Part 1

differences :: [Int] -> [Int]
differences (x0:x1:xs) = (x1-x0): differences (x1:xs)
differences _ = []

part1 :: [Int] -> Int
part1 xs = length (filter (>0) (differences xs))

-- Part 2

windows :: [Int] -> [Int]
windows (x0:x1:x2:xs) = (x0+x1+x2) : windows (x1:x2:xs)
windows _ = []

part2 :: [Int] -> Int
part2 xs = length (filter (>0) (differences (windows xs)))


-- Advent of Code 2020, day 9

module PuzzleInput where

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
  let nums = sort $ parseAll (some natural) input
  putStrLn ("Part 1: " ++ show (part1 nums))
  putStrLn ("Part 2: " ++ show (part2 nums))

-- Part 1

-- list of differences
differences :: [Int] -> [Int]
differences (x1:xs@(x2:_)) = (x2-x1) : differences xs
differences _ = []

-- count the occurrences of a number in a list
count :: [Int] -> Int -> Int
count xs y = length (filter (==y) xs)

part1 :: [Int] -> Int
part1 nums = let ns = 0:sort nums
                 ds = differences ns
                 ones = count ds 1
                 threes = count ds 3 + 1
             in ones * threes

-- Part 2

indices :: [Int] -> [Int]
indices xs = [i | i <- [0..length xs -1]]

reach :: [Int] -> Int -> [Int]
reach xs y = [i | i <- indices xs, xs!!i-y `elem` [1..3]]
             
counts :: [Int] -> [Int]
counts xs = cs where
  cs = [if i == length xs-1 then 1 else sum (map (cs!!) (reach xs (xs!!i)))
       | i <- indices xs]



part2 :: [Int] -> Int
part2 nums = (counts (0:nums ++ [last nums + 3])) !! 0

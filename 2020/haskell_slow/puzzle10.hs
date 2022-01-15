-- Advent of Code 2020, day 10

module PuzzleInput where

import System.Environment
import Data.List

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

valSum :: M.Map Int Int -> [Int] -> Int
valSum m [] = 0
valSum m (i:is) = M.findWithDefault 0 i m + valSum m is

paths :: [Int] -> M.Map Int Int
paths as = ps where
           ps = M.fromList ((0,1):[(i,valSum ps [i-3,i-2,i-1]) | i <- as])

part2 :: [Int] -> Int
part2 nums =
  let device = last nums + 3
      nums' = nums ++ [device]
  in (paths nums') M.! device


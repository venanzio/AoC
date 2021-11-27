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

-- Association list with indices as keys
index :: [a] -> [(Int,a)]
index = zip [0..]

-- finite map with indices as keys
indexMap :: [a] -> M.Map Int a
indexMap = M.fromList . index

-- indices of elements with differences <=3 from the given one (we assume xm sorted)
steps :: Int -> Int -> M.Map Int Int -> [Int]
steps i x xm = takeWhile (\k -> (xm M.! k - x) <= 3) [i+1 .. n]
               where (n,_) = M.findMax xm

-- for every index, count the arrangements starting at that element
--  use a dynamic programming approach
arrangements :: [Int] -> M.Map Int Int
arrangements xs = arrs where
  n = length xs -1
  xm = indexMap xs
  arrs = M.mapWithKey (\i x -> if i == n then 1 else sum (map (arrs M.!) (steps i x xm))) xm

part2 :: [Int] -> Int
part2 nums = (arrangements (0:nums ++ [last nums + 3])) M.! 0


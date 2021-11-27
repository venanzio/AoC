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
  let nums = parseAll (some natural) input
      (pre,post) = splitAt 25 nums
      Just r = noPSum pre post
      Just run = findRun nums r
      srun = sort run
  putStrLn ("Part 1: " ++ show r)
  putStrLn ("Part 2: " ++ show (head srun + last srun))


-- Part 1

-- pairSum xs y: check if y is the sum of twxo elements of xs
pairSum :: [Int] -> Int -> Bool
pairSum [] _ = False
pairSum (x:xs) y = (y-x) `elem` xs || pairSum xs y

-- check the puzzle property
-- return the first number that doesn't satisfy it
noPSum :: [Int] -> [Int] -> Maybe Int
noPSum xs [] = Nothing
noPSum xs (y:ys) = if pairSum xs y
                   then noPSum (tail xs ++ [y]) ys 
                   else Just y

-- Part 2

-- run xs y: initial segment of xs not smaller than y
run :: [Int] -> Int -> [Int]
run (x:xs) y
  | y<=0      = []
  | otherwise = x : run xs (y-x)  

-- find a run of contiguous numbers that add up to y
findRun :: [Int] -> Int -> Maybe [Int]
findRun [] y = Nothing
findRun xs y = let rs = run xs y
               in if sum rs == y
                  then Just rs
                  else findRun (tail xs) y


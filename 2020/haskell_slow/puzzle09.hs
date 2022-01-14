-- Advent of Code 2020, day 9

module PuzzleInput where

import System.Environment
import Data.List

import FunParser
import Control.Applicative

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
      run = sumRun [] 0 r nums  -- findRun nums r
      srun = sort run
  putStrLn ("Part 1: " ++ show r)
  putStrLn ("Part 2: " ++ show (head srun + last srun))


-- Part 1

-- pairSum xs y: check if y is the sum of two elements of xs
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

-- rs = current run, s = its sum, y = target, nums = list of numbers to use
sumRun :: [Int] -> Int -> Int -> [Int] -> [Int]
sumRun rs s y nums
  | s == y = rs
  | s > y  = sumRun (tail rs) (s-head rs) y nums
  | s < y  = let x = head nums
             in  sumRun (rs++[x]) (s+x) y (tail nums)



module PuzzleInput where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

puzzle :: String -> IO Int
puzzle fileName = do
  input <- readFile fileName
  let nums = parseAll (some natural) input
      (pre,post) = splitAt 25 nums
      Just r = noPSum pre post
      Just run = findRun nums r
      srun = sort run
  return (head srun + last srun)


-- Part 1

pairSum :: [Int] -> Int -> Bool
pairSum [] _ = False
pairSum (x:xs) y = (y-x) `elem` xs || pairSum xs y

noPSum :: [Int] -> [Int] -> Maybe Int
noPSum xs [] = Nothing
noPSum xs (y:ys) = if pairSum xs y
                   then noPSum (tail xs ++ [y]) ys 
                   else Just y

-- Part 2

run :: [Int] -> Int -> [Int]
run (x:xs) y
  | y<=0      = []
  | otherwise = x : run xs (y-x)  
    
findRun :: [Int] -> Int -> Maybe [Int]
findRun [] y = Nothing
findRun xs y = let rs = run xs y
               in if sum rs == y
                  then Just rs
                  else findRun (tail xs) y


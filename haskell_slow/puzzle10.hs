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
  let nums = parseAll numbers input
      ns' = sort nums
      ns = 0:ns' ++ [last ns' + 3]
      ds = differences ns
      ones = count ds 1
      threes = count ds 3
  putStrLn ("Part 1: " ++ show (ones * threes))
  putStrLn ("Part 2: " ++ show (counts ns!!0))

numbers :: Parser [Int]
numbers = many natural

-- Part 1

differences :: [Int] -> [Int]
differences (x1:x2:xs) = (x2-x1) : differences (x2:xs)
differences _ = []

count :: [Int] -> Int -> Int
count xs y = length (filter (==y) xs)

-- Part 2

indices :: [Int] -> [Int]
indices xs = [i | i <- [0..length xs -1]]

reach :: [Int] -> Int -> [Int]
reach xs y = [i | i <- indices xs, xs!!i-y `elem` [1..3]]
             
counts :: [Int] -> [Int]
counts xs = cs where
  cs = [if i == length xs-1 then 1 else sum (map (cs!!) (reach xs (xs!!i)))
       | i <- indices xs]




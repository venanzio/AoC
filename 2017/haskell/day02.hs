-- Advent of Code 2017, day 2

module Main where

import System.Environment
import Control.Applicative

import FunParser

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  putStrLn "Advent of Code 2017, day 2"
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pInput :: Parser [[Int]]
pInput = pLines (some natural)

-- Part 1

part1 :: [[Int]] -> Int
part1 xs = sum [maximum(l)-minimum(l) | l <- xs]

-- Part 2

evenDiv :: [Int] -> (Int,Int)
evenDiv l = head [(x,y) | x<-l, y<-l, x/=y, x `rem` y == 0]

part2 :: [[Int]] -> Int
part2 xs = sum [x `div` y | (x,y) <- map evenDiv xs]

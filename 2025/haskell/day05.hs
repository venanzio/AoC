-- Advent of Code 2025, day 5
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Control.Applicative

import FunParser
import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let (rs,as) = parseAll pInput input
      ranges = intsRange rs
  putStrLn ("Part 1: " ++ show (part1 ranges as))
  putStrLn ("Part 2: " ++ show (part2 ranges))

-- Parsing the input

pRange :: Parser (Int,Int)
pRange = do x <- natural
            string "-"
            y <- natural
            return (x,y)

pInput :: Parser ([(Int,Int)],[Int])
pInput = do ranges <- some pRange
            available <- some natural
            return (ranges,available)

-- Part 1

part1 :: [(Int,Int)] -> [Int] -> Int
part1 r = length . filter (flip inRange r)

-- Part 2

part2 :: [(Int,Int)] -> Int
part2 = sizeRange

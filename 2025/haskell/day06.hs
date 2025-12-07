-- Advent of Code 2025, day 6
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Data.Char
import Control.Applicative
import qualified Data.Map as M

import FunParser
import AoCTools

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

pOper :: Parser ([Int] -> Int)
pOper = (symbol "+" >> return (sum)) <|> (symbol "*" >> return (product))

pInput :: Parser ([String], [[Int]->Int])
pInput = do ls <- some line
            let ops = parseAll (some pOper) (last ls)
            return (init ls,ops)

-- Part 1

nums1 :: [String] -> [[Int]]
nums1 ls = transpose (map (parseAll (some natural)) ls) 

part1 :: ([String], [[Int]->Int]) -> Int
part1 (ls,ops) = sum $ zipWith ($) ops (nums1 ls)

-- Part 2

part2 :: ([String], [[Int]->Int]) -> Int
part2 _ = 2

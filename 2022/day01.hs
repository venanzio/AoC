-- Advent of Code 2022, day 1
--   Venanzio Capretta

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

pData :: Parser [Int]
pData = many integer

pInput :: Parser [[Int]]
pInput = blocks pData

-- Part 1

part1 :: [[Int]] -> Int
part1 xs = maximum (map sum xs)

-- Part 2

part2 :: [[Int]] -> Int
part2 = sum . take 3 . reverse . sort . map sum

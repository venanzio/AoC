-- Advent of Code 2023, day 9
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

pData :: Parser [Int]
pData = many integer

pInput :: Parser [[Int]]
pInput = pLines pData

-- Part 1

differences :: [Int] -> [Int]
differences [] = []
differences [x] = []
differences (x0:x1:l) = x1-x0 : differences (x1:l)

nextVal :: [Int] -> Int
nextVal l = if all (==0) l then 0
            else last l + nextVal (differences l)

part1 :: [[Int]] -> Int
part1 = sum . map nextVal

-- Part 2

prevVal :: [Int] -> Int
prevVal l = if all (==0) l then 0
            else head l - prevVal (differences l)

part2 :: [[Int]] -> Int
part2 = sum . map prevVal

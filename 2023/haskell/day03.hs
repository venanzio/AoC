-- Advent of Code 2023, day 3
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
  let ls = lines input
      pnums = partNums ls
      symbs = symbols ls
  putStrLn (show pnums)
  putStrLn ("Part 1: " ++ show (part1 pnums symbs))
  putStrLn ("Part 2: " ++ show (part2 pnums symbs))

-- Parsing the input

partNums :: [String] -> [(Int,Int,Int)]
partNums = undefined
    
symbols :: [String] -> [(Int,Int)]
symbols ls = symCoords 0 0 where
  symCoords x y | y >= length ls      = []
                | x >= length (ls!!y) = symCoords 0 (y+1)
                | ls!!y!!x == '.' || isDigit (ls!!y!!x)
                                      = symCoords (x+1) y
                | otherwise           = (x,y) : symCoords (x+1) y

-- Part 1

part1 :: [(Int,Int,Int)] -> [(Int,Int)] -> Int
part1 pnums symbs = 1

-- Part 2

part2 :: [(Int,Int,Int)]-> [(Int,Int)] -> Int
part2 pnums symbs = 2

-- Advent of Code 2024, day 6
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
-- import Data.Char
-- import Control.Applicative
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
      maxX = length (ls!!1)
      maxY = length ls
      map = mMap ls
      start = mFind '#' map
  putStrLn (show start)
  putStrLn ("Part 1: " ++ show (part1 maxX maxY map))
  putStrLn ("Part 2: " ++ show (part2 maxX maxY map))

-- Part 1

part1 :: Int -> Int -> Map2D Char -> Int
part1 maxX maxY map = 1

-- Part 2

part2 :: Int -> Int -> Map2D Char -> Int
part2 maxX maxY map = 2

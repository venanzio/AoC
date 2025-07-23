-- Advent of Code 2024, day 4
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
  let xs = mMap (lines input)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Part 1

part1 :: Map2D Char -> Int
part1 = mOccurrences "XMAS"

-- Part 2

xmas = stringsMap ["M.S",".A.","M.S"]

patterns = [xmas, hMirror xmas, vMirror xmas, vMirror (hMirror xmas)]


part2 :: Map2D Char -> Int
part2 m = sum (map (\sub -> subOccurrences sub m) patterns)

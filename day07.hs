-- Advent of Code 2021, day 7

module Main where

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
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser ()
pData = return ()

pInput :: Parser [Int]
pInput = seqSep natural ","

-- Part 1


dist :: [Int] -> Int -> Int
dist xs y = sum (map (abs.(y-)) xs)

part1 :: [Int] -> Int
part1 xs = minimum [dist xs y | y <- [0..(maximum xs)]]

-- Part 2

dst :: Int -> Int -> Int
dst y x = let d = abs (y-x) in d * (d+1) `div` 2

dist2 :: [Int] -> Int -> Int
dist2 xs y = sum (map (dst y) xs)

part2 :: [Int] -> Int
part2 xs = minimum [dist2 xs y | y <- [0..(maximum xs)]]


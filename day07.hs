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

dist1 :: Int -> Int -> Int
dist1 y x = abs (y-x)

totDist :: (Int -> Int -> Int) -> [Int] -> Int -> Int
totDist dist xs y = sum (map (dist y) xs)

minDist :: (Int -> Int -> Int) -> [Int] -> Int
minDist dist xs = minimum [totDist dist xs y | y <- [0..(maximum xs)]]

part1 :: [Int] -> Int
part1 =  minDist dist1

-- Part 2

dist2 :: Int -> Int -> Int
dist2 y x = let d = dist1 y x in d * (d+1) `div` 2

part2 :: [Int] -> Int
part2 = minDist dist2


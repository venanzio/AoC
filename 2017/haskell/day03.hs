-- Advent of Code 2017, day 3

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
  putStr("input number: ")
  input <- getLine
  let x = read input
  puzzle x

input = 289326 :: Int

puzzle :: Int -> IO ()
puzzle x = do
  putStrLn ("Part 1: " ++ show (part1 x))
  putStrLn ("Part 2: " ++ show (part2 x))

-- Part 1

-- "inner" square, return n s.t. (2n-1)^2 < x <= (2n+1)^2
square_lt :: Int -> Int
square_lt x = sqlt 0
  where sqlt n = if x <= (2*n+1)^2 then n else sqlt (n+1)

part1 :: Int -> Int
part1 _ = 1

-- Part 2

part2 :: Int -> Int
part2 _ = 2

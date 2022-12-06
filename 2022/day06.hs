-- Advent of Code 2022, day 6
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
  putStrLn ("Part 1: " ++ show (part1 input))
  putStrLn ("Part 2: " ++ show (part2 input))

-- Part 1

norep :: String -> Bool
norep xs = let n = length xs in length (nub xs) == n

marker :: Int -> String -> String -> Int
marker n xs (y:ys) =
  if norep xs then n
              else marker (n+1) (tail xs++[y]) ys

findMarker :: Int -> String -> Int
findMarker n ys = let (xs,zs) = splitAt n ys
                  in marker n xs zs

part1 :: String -> Int
part1 = findMarker 4

-- Part 2

part2 :: String -> Int
part2 = findMarker 14


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
  let xs = input -- parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser ()
pData = return ()

pInput :: Parser [()]
pInput = pLines pData

-- Part 1

norep :: String -> Bool
norep xs = length (nub xs) == 4

marker :: Int -> String -> String -> Int
marker n xs (y:ys) =
  if norep xs then n
              else marker (n+1) (tail xs++[y]) ys


part1 :: String -> Int
part1 ys = marker 4 (take 4 ys) (drop 4 ys)

-- Part 2

norep2 :: String -> Bool
norep2 xs = length (nub xs) == 14

marker2 :: Int -> String -> String -> Int
marker2 n xs (y:ys) =
  if norep2 xs then n
               else marker2 (n+1) (tail xs++[y]) ys


part2 :: String -> Int
part2 ys = marker2 14 (take 14 ys) (drop 14 ys)


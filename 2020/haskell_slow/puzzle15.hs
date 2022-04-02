-- Advent of Code 2020, day 15

{- This is based on Van Eck's sequence
   OEIS: A181391 - https://oeis.org/A181391
   Nuberphile: https://youtu.be/etMJxB-igrc
-}

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

-- input given as Haskell list, for example: "[15,12,0,14,3,1]"
main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

input = [15,12,0,14,3,1]

puzzle :: String -> IO ()
puzzle input = do
  let xs = (read input)
  putStrLn ("input: " ++ show xs)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser ()
pData = return ()

pInput :: Parser [()]
pInput = pLines pData

-- Part 1

part1 :: [Int] -> Int
part1 _ = 1

-- Part 2

part2 :: [Int] -> Int
part2 _ = 2

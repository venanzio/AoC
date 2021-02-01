-- Advent of Code 2020, day 5

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let bps = words input
      ids = map seatID bps
  putStrLn ("Part 1: " ++ show (maximum ids))
  putStrLn ("Part 2: " ++ show (findMissing (map seatID bps)))

-- value of a number in birary representation (most significant digit first)
bin :: [Bool] -> Int
bin = foldl (\r b -> 2*r + if b then 1 else 0) 0

row :: String -> Int
row = bin . map rbool where
  rbool 'F' = False
  rbool 'B' = True

col :: String -> Int
col = bin . map cbool where
  cbool 'L' = False
  cbool 'R' = True 

seat :: String -> (Int,Int)
seat s = let (r,c) = splitAt 7 s
         in (row r, col c)

seatID :: String -> Int
seatID s = let (r,c) = seat s
           in 8*r+c

-- Find the missing seat ID number
findMissing :: [Int] -> Int
findMissing seats = fmiss (head sts + 1) (tail sts) where
  sts = sort seats
  fmiss n [] = error "no missing seat"
  fmiss n (m:ms) = if n == m then fmiss (n+1) ms
                             else n



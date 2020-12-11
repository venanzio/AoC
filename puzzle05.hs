module PuzzleInput where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

puzzle :: String -> IO Int
puzzle fileName = do
  input <- readFile fileName
  let bps = words input
      ids = map seatID bps
  return (findMissing (map seatID bps))

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


findMissing :: [Int] -> Int
findMissing seats = fmiss (head sts + 1) (tail sts) where
  sts = sort seats
  fmiss n [] = error "no missing seat"
  fmiss n (m:ms) = if n == m then fmiss (n+1) ms
                             else n



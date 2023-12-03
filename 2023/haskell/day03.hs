-- Advent of Code 2023, day 3
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
  let ls = lines input
      pnums = partNums ls
      symbs = symbols ls
  putStrLn (show pnums)
  putStrLn (show symbs)
  putStrLn ("Part 1: " ++ show (part1 pnums symbs))
  putStrLn ("Part 2: " ++ show (part2 pnums symbs))

-- Parsing the input

-- log in base 10 to compute number of digits
numDigits :: Int -> Int
numDigits x | x < 10 = 1
            | otherwise = 1 + numDigits (x `div` 10)

readNum :: String -> Int
readNum = read . takeWhile isDigit

partNums :: [String] -> [(Int,Int,Int)]
partNums ls = pnCoords 0 0 where
  pnCoords x y | y >= length ls      = []
               | x >= length (ls!!y) = pnCoords 0 (y+1)
               | isDigit (ls!!y!!x)  = let n = readNum (drop x (ls!!y))
                                       in (n,x,y) : pnCoords (x+numDigits n) y
               | otherwise           = pnCoords (x+1) y
    
symbols :: [String] -> [(Int,Int)]
symbols ls = symCoords 0 0 where
  symCoords x y | y >= length ls      = []
                | x >= length (ls!!y) = symCoords 0 (y+1)
                | ls!!y!!x == '.' || isDigit (ls!!y!!x)
                                      = symCoords (x+1) y
                | otherwise           = (x,y) : symCoords (x+1) y

-- Part 1

neighbours :: (Int,Int,Int) -> [(Int,Int)]
neighbours (n,x,y) = let d = numDigits n in
  (x-1,y) : (x+d,y) : [(x+i,y-1) | i <- [-1..d]] ++ [(x+i,y+1) | i <- [-1..d]]  

nearSym :: (Int,Int,Int) -> [(Int,Int)] -> Bool
nearSym num ss = or [xy `elem` ss | xy <- neighbours num]

numN (n,_,_) = n

part1 :: [(Int,Int,Int)] -> [(Int,Int)] -> Int
part1 pnums symbs = sum [numN num | num <- pnums, nearSym num symbs]

-- Part 2

part2 :: [(Int,Int,Int)]-> [(Int,Int)] -> Int
part2 pnums symbs = 2

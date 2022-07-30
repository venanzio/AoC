-- Advent of Code 2017, day 6

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
  let xs = parseAll pInput input
  putStrLn ("Input: " ++ show xs)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pInput :: Parser [Int]
pInput = some natural

-- Part 1

splitOn :: Int -> [Int] -> ([Int],[Int])
splitOn x [] = ([],[])
splitOn x (y:ys)
  | x==y      = ([0],ys)
  | otherwise = let (us,vs) = splitOn x ys
                in (y:us,vs)

redistribute :: Int -> ([Int],[Int]) -> [Int]
redistribute 0 (xs,ys) = xs++ys
redistribute n (x:xs,[]) = redistribute (n-1) ([x+1],xs)
redistribute n (xs,y:ys) = redistribute (n-1) (xs++[y+1],ys)

redStep :: [Int] -> [Int]
redStep xs = let m = maximum xs
             in redistribute m (splitOn m xs)

redRounds :: [Int] -> [[Int]] -> Int
redRounds xs yss = let us = redStep xs
                   in if us `elem` yss then length yss +1
                                       else redRounds us (xs:yss)
                                            
part1 :: [Int] -> Int
part1 xs = redRounds xs []

-- Part 2

part2 :: [Int] -> Int
part2 _ = 2

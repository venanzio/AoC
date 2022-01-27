-- Advent of Code 2017, day ?

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
  putStrLn "Advent of Code 2017, day 4"  
  input <- readFile fileName
  let xs = parseAll digits input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Part 1

sumDig :: [Int] -> Int
sumDig (x:xs@(y:_)) = if x==y then x + sumDig xs else sumDig xs
sumDig _ = 0

captcha :: [Int] -> Int
captcha xs = if last xs == head xs then head xs + sumDig xs else sumDig xs


part1 :: [Int] -> Int
part1 = captcha

-- Part 2

matchCount :: [Int] -> [Int] -> Int
matchCount (x:xs) (y:ys) = if x==y then x + matchCount xs ys else matchCount xs ys
matchCount _ _ = 0

part2 :: [Int] -> Int
part2 nums = let (xs,ys) = splitAt (length nums `div` 2) nums
             in 2 * matchCount xs ys

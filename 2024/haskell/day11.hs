-- Advent of Code 2024, day 11
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
-- import Data.Char
import Control.Applicative
-- import qualified Data.Map as M

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
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pInput :: Parser [Int]
pInput = some natural

-- Part 1

blink :: Int -> Int -> Int
blink 0 _ = 1
blink n 0 = blink (n-1) 1
blink n x = let d = dig10 x
                (x0,x1) = quotRem x (10^(d `div` 2))
            in if even d then blink (n-1) x0 + blink (n-1) x1
                         else blink (n-1) (2024*x)
                   
part1 :: [Int] -> Int
part1 = sum . map (blink 25)

-- Part 2

part2 :: [Int] -> Int
part2 _ = 2

-- Advent of Code 2024, day 11
--  Venanzio Capretta

module Main where

import System.Environment
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
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pInput :: Parser [Int]
pInput = some natural

-- Part 1

{- Brute force solution:
blink :: Int -> Int -> Int
blink 0 _ = 1
blink n 0 = blink (n-1) 1
blink n x = let d = dig10 x
                (x0,x1) = quotRem x (10^(d `div` 2))
            in if even d then blink (n-1) x0 + blink (n-1) x1
                         else blink (n-1) (2024*x)
-}

-- Solution using dynamic programming

blink :: M.Map  (Int,Int) Int -> (Int,Int) -> (Int, M.Map  (Int,Int) Int)
blink m b =
  case M.lookup b m of
    Just y -> (y,m)
    Nothing -> case b of
      (0,x) -> (1, M.insert b 1 m)
      (n,0) -> let (y0,m0) = blink m (n-1,1)
               in (y0, M.insert b y0 m)
      (n,x) -> if even d then (y0+y1, M.insert b (y0+y1) m1)
                         else (y2, M.insert b y2 m0)
               where d = dig10 x
                     (x0,x1) = quotRem x (10^(d `div` 2))
                     (y0,m0) = blink m (n-1,x0)
                     (y1,m1) = blink m0 (n-1,x1)
                     (y2,m2) = blink m0 (n-1,2024*x)
                               
blinks :: M.Map  (Int,Int) Int -> Int -> [Int] -> Int
blinks _ _ [] = 0
blinks m n (x:xs) = let (y,m0) = blink m (n,x) in y + blinks m0 n xs

part1 :: [Int] -> Int
part1 = blinks M.empty 25

-- Part 2

part2 :: [Int] -> Int
part2 = blinks M.empty 75

-- Advent of Code 2025, day 2
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Data.Char
import Control.Applicative
import qualified Data.Map as M

import FunParser_old
import AoCTools_old

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

pData :: Parser (Int,Int)
pData = do x <- natural
           symbol "-"
           y <- natural
           return (x,y)

pInput :: Parser [(Int,Int)]
pInput = someSepStr pData ","

-- Part 1

log10 :: Int -> Int
log10 0 = 0
log10 x = 1 + log10 (x `div` 10)

splitUp :: Int -> (Int,Int)
splitUp x = if even n then (x1,x2) else (x',x') where
  n = log10 x
  x' = 10^(n `div` 2)
  x1 = x `div` x'
  x2 = max (x - x1 * x') (x' `div` 10)

splitDown :: Int -> (Int,Int)
splitDown x = if even n then (x1,x2) else (x'-1,x'-1) where
  n = log10 x
  x' = 10^(n `div` 2)
  x1 = x `div` x'
  x2 = x - x1 * x'

invUp :: Int -> Int
invUp x | odd n = x'
        | x1 >= x2 = x1
        | otherwise = x1+1
  where n = log10 x
        x' = 10^(n `div` 2)
        x1 = x `div` x'
        x2 = x - x1 * x'

invDown :: Int -> Int
invDown x | odd n = x'-1
          | x1 <= x2 = x1
          | otherwise = x1-1
  where n = log10 x
        x' = 10^(n `div` 2)
        x1 = x `div` x'
        x2 = x - x1 * x'

dupl :: Int -> Int
dupl x = x * (1 + 10^(log10 x))

invalid :: Int -> Int -> [Int]
invalid x y = map dupl [invUp x .. invDown y] where
  (x1,x2) = splitUp x
  (y1,y2) = splitDown y
  

part1 :: [(Int,Int)] -> Int
part1 xys = sum $ concat [invalid x y | (x,y) <- xys]

-- Part 2

res :: Int -> Int -> Int
res x m = x - (x `div` 10^m) * 10^m

repDigs :: Int -> Int -> Bool
repDigs x m = isRepeat x where
  rx = res x m
  isRepeat 0 = True
  isRepeat y = res y m == rx && isRepeat (y `div` 10^m)

repetition :: Int -> Bool
repetition x = or [repDigs x m | m <- [2..log10 x-1]]

repRange :: Int -> Int -> [Int]
repRange x y = filter repetition [x..y]
                    
part2 :: [(Int,Int)] -> Int
part2 xys = sum $ concat [repRange x y | (x,y) <- xys]

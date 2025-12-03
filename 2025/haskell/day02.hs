-- Advent of Code 2025, day 2
--  Venanzio Capretta

module Main where

import System.Environment

import FunParser

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

{-
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
-}
----

-- take the first n digits of x

takeDigs :: Int -> Int -> Int
takeDigs n x = x `div` 10^(log10 x - n)

-- iterate given digits k times
itDigs :: Int -> Int -> Int
itDigs 0 x = 1
itDigs 1 x = x
itDigs k x = (itDigs (k-1) x) * 10^(log10 x) + x

-- minimum duplicate number larger than x
firstDupl :: Int -> Int
firstDupl x = if itDigs 2 y < x then y+1 else y
  where  n = log10 x
         m = n `div` 2
         y = if even n then takeDigs m x else 10^m

-- maximum duplicate number smaller than x
lastDupl :: Int -> Int
lastDupl x = if itDigs 2 y > x then y-1 else y
  where  n = log10 x
         m = n `div` 2
         y = if even n then takeDigs m x else 10^m-1

-- all invalid IDs in a range
invalid :: Int -> Int -> [Int]
invalid x y = [itDigs 2 z | z <- [firstDupl x .. lastDupl y]]

----

part1 :: [(Int,Int)] -> Int
part1 xys = sum $ concat [invalid x y | (x,y) <- xys]

-- Part 2

res :: Int -> Int -> Int
res x m = x - (x `div` 10^m) * 10^m

repDigs :: Int -> Int -> Bool
repDigs x m = rx >= 10^(m-1) && repeats x >= 2 where
  n = log10 x
  rx = res x m
  repeats 0 = 0
  repeats y = if res y m == rx then 1+repeats (y `div` 10^m) else -n
  isRepeat 0 = True
  isRepeat y = res y m == rx && isRepeat (y `div` 10^m)

repetition :: Int -> Bool
repetition x = or [repDigs x m | m <- [1..log10 x-1]]

repRange :: Int -> Int -> [Int]
repRange x y = filter repetition [x..y]
                    
part2 :: [(Int,Int)] -> Int
part2 xys = sum $ concat [repRange x y | (x,y) <- xys]

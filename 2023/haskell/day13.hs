-- Advent of Code 2023, day 13
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName >>= return.linesN
  let ms = mirrors input
  putStrLn (show (rCol (ms!!0)))
  putStrLn ("Part 1: " ++ show (part1 ms))
  putStrLn ("Part 2: " ++ show (part2 ms))

-- Parsing the input

linesN :: String -> [String]
linesN "" = []
linesN s = let (l,s') = break (=='\n') s in l:
  if s'=="" then [] else linesN (tail s')

mirrors :: [String] -> [[String]]
mirrors [] = []
mirrors s = let (m,s') = break (all (==' ')) s in m:
  if s'==[] then [] else mirrors (tail s')

-- Part 1

reflectionR :: Eq a => [a] -> Int -> Bool
reflectionR m i =
  let (m0,m1) = splitAt i m 
      n = min (length m0) (length m1)
      m0' = take n (reverse m0)
      m1' = take n m1
  in all (\(l0,l1) -> l0==l1) (zip m0' m1')

rRow :: Eq a => [a] -> Maybe Int
rRow m = find (reflectionR m) [1..length m - 1] 

rCol :: [String] -> Maybe Int
rCol = rRow . transpose

mScore :: [String] -> Int
mScore m = case rRow m of
  Just i -> 100*i
  Nothing -> case rCol m of
    Just i -> i
    Nothing -> error "no reflection"

part1 :: [[String]] -> Int
part1 ms = sum (map mScore ms)

-- Part 2

diffPoints :: Eq a => [a] -> [a] -> Int
diffPoints l0 l1 = length (filter (\(x0,x1) -> x0/=x1) (zip l0 l1))

rDiffs :: [String] -> Int -> Int
rDiffs m i = 
  let (m0,m1) = splitAt i m 
      n = min (length m0) (length m1)
      m0' = take n (reverse m0)
      m1' = take n m1
  in sum $ map (\(l0,l1) -> diffPoints l0 l1) (zip m0' m1')

rSmudge :: [String] -> Int -> Bool
rSmudge m i = rDiffs m i == 1

rSRow :: [String] -> Maybe Int
rSRow m = find (rSmudge m) [1..length m - 1] 

rSCol :: [String] -> Maybe Int
rSCol = rSRow . transpose

mSScore :: [String] -> Int
mSScore m = case rSRow m of
  Just i -> 100*i
  Nothing -> case rSCol m of
    Just i -> i
    Nothing -> error "no reflection"

part2 :: [[String]] -> Int
part2 ms = sum (map mSScore ms)

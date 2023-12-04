-- Advent of Code 2023, day 4
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
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser ([Int],[Int])
pData = do symbol "Card"
           natural
           symbol ":"
           winning <- many natural
           symbol "|"
           values <- many natural
           return (winning,values)
           
pInput :: Parser [([Int],[Int])]
pInput = pLines pData

-- Part 1

wins :: ([Int],[Int]) -> Int
wins (ws,vs) = length (filter (\v -> v `elem` ws) vs)

part1 :: [([Int],[Int])] -> Int
part1 xs = sum [2^(wins x - 1) | x <- xs, wins x > 0]

-- Part 2

copyCards :: [Int] -> Int -> Int -> [Int]
copyCards l w c = let (l0,l1) = splitAt w l in
  (map (+ c) l0) ++ l1

winCopy :: [([Int],[Int])] -> Int -> [Int] -> Int
winCopy xs s [] = s
winCopy (x:xs) s (c:cards) = winCopy xs (s+c) (copyCards cards (wins x) c)
    
part2 :: [([Int],[Int])] -> Int
part2 xs = winCopy xs 0 (take (length xs) (repeat 1))

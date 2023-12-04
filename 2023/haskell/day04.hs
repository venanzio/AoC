-- Advent of Code 2023, day 4
--  Venanzio Capretta

module Main where

import System.Environment
import FunParser
import Control.Applicative
import AoCTools (zipWithLong)

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
      ws = map wins xs
  putStrLn ("Part 1: " ++ show (part1 ws))
  putStrLn ("Part 2: " ++ show (part2 ws))

-- Parsing the input

type Card = ([Int],[Int])

pData :: Parser Card
pData = do symbol "Card" >> natural >> symbol ":"
           winning <- many natural
           symbol "|"
           values <- many natural
           return (winning,values)
           
pInput :: Parser [Card]
pInput = pLines pData

-- Part 1

wins :: Card -> Int
wins (ws,vs) = length (filter (\v -> v `elem` ws) vs)

worth :: Int -> Int
worth w = if w > 0 then 2^(w - 1) else 0

part1 :: [Int] -> Int
part1 = sum . map worth

-- Part 2

copyCards :: Int -> Int -> [Int] -> [Int]
copyCards w c = zipWithLong (+) (take w (repeat c))

winCopy :: [Int] -> Int -> [Int] -> Int
winCopy ws s [] = s
winCopy (w:ws) s (c:cards) = winCopy ws (s+c) (copyCards w c cards)
    
part2 :: [Int] -> Int
part2 ws = winCopy ws 0 (take (length ws) (repeat 1))

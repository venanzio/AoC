-- Advent of Code 2024, day 3
--  Venanzio Capretta

module Main where

import System.Environment
import Control.Applicative

import FunParser

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (puzzle1 xs))
  putStrLn ("Part 2: " ++ show (puzzle2 xs))
  
-- Parsing the input

mulOp :: Parser Int
mulOp = do symbol "mul"
           parens (integer >>= \x -> symbol "," >> integer >>= \y -> return (x*y))

mulDo :: Parser (Either Int Bool)
mulDo = (mulOp >>= return.Left) <|>
        (symbol "don\'t()" >> return (Right False)) <|>
        (symbol "do()" >> return (Right True))

pInput :: Parser [Either Int Bool]
pInput = only (some (skipTo mulDo))

-- Part 1

puzzle1 :: [Either Int Bool] -> Int
puzzle1 xs = sum [x | Left x <- xs]

-- Part 2

puzzle2 :: [Either Int Bool] -> Int
puzzle2 [] = 0
puzzle2 (Left x:xs) = x + puzzle2 xs
puzzle2 (Right True:xs) = puzzle2 xs
puzzle2 (Right False:xs) = puzzle2 (snd $ break (==Right True) xs)


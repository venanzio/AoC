-- Advent of Code 2025, day 6
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

pOper :: Parser (Int -> Int -> Int)
pOper = (symbol "+" >> return (*)) <|> (symbol "*" >> return (*))

pInput :: Parser ([[Int]], [Int->Int->Int])
pInput = do numCols <- pLines (some natural)
            let numRows = transpose numCols
            ops <- some pOper
            return (numRows,ops)

-- Part 1

part1 :: ([[Int]], [Int->Int->Int]) -> Int
part1 _ = 1

-- Part 2

part2 :: ([[Int]], [Int->Int->Int]) -> Int
part2 _ = 2

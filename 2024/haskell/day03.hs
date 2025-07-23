-- Advent of Code 2024, day 3
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
-- import Data.Char
import Control.Applicative
-- import qualified Data.Map as M

import FunParser
-- import AoCTools

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

mulOp :: Parser Int
mulOp = do symbol "mul"
           parens (integer >>= \x -> symbol "," >> integer >>= \y -> return (x*y))

pData :: Parser ()
pData = return ()

pInput :: Parser [Int]
pInput = some (skipTo mulOp)

-- Part 1

part1 :: [Int] -> Int
part1 = sum

-- Part 2

part2 :: [Int] -> Int
part2 _ = 2

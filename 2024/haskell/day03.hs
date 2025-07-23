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
  let xs1 = parseAll parse1 input
  putStrLn ("Part 1: " ++ show (sum xs1))
  let xs2 = parseAll parse2 input
  putStrLn ("Part 2: " ++ show (sum xs2))

-- Parsing the input

mulOp :: Parser Int
mulOp = do symbol "mul"
           parens (integer >>= \x -> symbol "," >> integer >>= \y -> return (x*y))

-- Part 1

parse1 :: Parser [Int]
parse1 = only (some (skipTo mulOp))

-- Part 2

doMul :: Parser Int
doMul = skipTo (mulOp <|> (symbol "don\'t()" >> dont))

dont :: Parser Int
dont = skipTo (symbol "do()") >> doMul

parse2 :: Parser [Int]
parse2 = only (some (skipTo doMul))

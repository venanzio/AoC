-- Advent of Code 2024, day 5
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
  let (rs,pss) = parseAll pInput input
  putStrLn (show rs)
  putStrLn (show pss)
  putStrLn ("Part 1: " ++ show (part1 rs pss))
  putStrLn ("Part 2: " ++ show (part2 rs pss))

-- Parsing the input

pRule :: Parser (Int,Int)
pRule = do p1 <- natural
           symbol "|"
           p2 <- natural
           return (p1,p2)

pRules :: Parser [(Int,Int)]
pRules = some pRule

pPages :: Parser [Int]
pPages = someSepStr natural ","

pInput :: Parser ([(Int,Int)],[[Int]])
pInput = do rs <- pRules
            pss <- pLines pPages
            return (rs,pss)

-- Part 1

part1 :: [(Int,Int)] -> [[Int]] -> Int
part1 rs pss = 1

-- Part 2

part2 :: [(Int,Int)] -> [[Int]] -> Int
part2 rs pss = 2

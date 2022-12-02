-- Advent of Code 2022, day 2
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

pData :: Parser (String,String)
pData = do opponent <- word
           you <- word
           return (opponent,you)

pInput :: Parser [(String,String)]
pInput = many pData

-- Part 1

shape :: String -> Int
shape "X" = 1
shape "Y" = 2
shape "Z" = 3

outcome :: String -> String -> Int
outcome "C" "X" = 6
outcome "B" "Z" = 6
outcome "A" "Y" = 6
outcome "A" "Z" = 0
outcome "C" "Y" = 0
outcome "B" "X" = 0
outcome a x = 3

guide :: (String,String) -> Int
guide (a,x) = shape x + outcome a x


part1 :: [(String,String)] -> Int
part1 = sum . map guide

-- Part 2

choose :: String -> String -> String
choose "A" "X" = "Z"
choose "A" "Y" = "X"
choose "A" "Z" = "Y"
choose "B" "X" = "X"
choose "B" "Y" = "Y"
choose "B" "Z" = "Z"
choose "C" "X" = "Y"
choose "C" "Y" = "Z"
choose "C" "Z" = "X"




part2 :: [(String,String)] -> Int
part2 = part1 . map (\(a,x) -> (a,choose a x))

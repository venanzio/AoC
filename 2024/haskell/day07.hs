-- Advent of Code 2024, day 7
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
  putStrLn (show xs)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser (Int,[Int])
pData = do s <- natural
           symbol ":"
           xs <- some natural
           return (s,xs)

pInput :: Parser [(Int,[Int])]
pInput = pLines pData

-- Part 1

part1 :: [(Int,[Int])] -> Int
part1 _ = 1

-- Part 2

part2 :: [(Int,[Int])] -> Int
part2 _ = 2

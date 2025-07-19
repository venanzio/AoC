-- Advent of Code 2024, day 1
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
-- import Data.Char
-- import Control.Applicative
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

pData :: Parser (Int,Int)
pData = do a <- natural
           b <- natural
           return (a,b)

pInput :: Parser ([Int],[Int])
pInput = do pairs <- pLines pData
            return (unzip pairs)

-- Part 1

part1 :: ([Int],[Int]) -> Int
part1 (as,bs) = sum $ zipWith (\a b -> abs (a-b)) (sort as) (sort bs)
      

-- Part 2

part2 :: ([Int],[Int]) -> Int
part2 _ = 2

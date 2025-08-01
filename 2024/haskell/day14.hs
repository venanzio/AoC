-- Advent of Code 2024, day 14
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
-- import Data.Char
-- import Control.Applicative
-- import qualified Data.Map as M

import FunParser
import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let rs = parseAll pInput input
  putStrLn (show rs)
  putStrLn ("Part 1: " ++ show (part1 rs))
  putStrLn ("Part 2: " ++ show (part2 rs))

-- Parsing the input

pIntPair :: Parser Point
pIntPair = do x <- integer
              symbol ","
              y <- integer
              return (x,y)

pData :: Parser (Point,Direction)
pData = do symbol "p="
           p <- pIntPair
           symbol "v="
           v <- pIntPair
           return (p,v)

pInput :: Parser [(Point,Direction)]
pInput = pLines pData

-- Part 1

part1 :: [(Point,Direction)] -> Int
part1 _ = 1

-- Part 2

part2 :: [(Point,Direction)] -> Int
part2 _ = 2

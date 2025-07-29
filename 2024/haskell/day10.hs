-- Advent of Code 2024, day 10
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
-- import Data.Char
import Control.Applicative
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
  let map = parseAll pInput input
  putStrLn (show map)
  putStrLn ("Part 1: " ++ show (part1 map))
  putStrLn ("Part 2: " ++ show (part2 map))

-- Parsing the input

pData :: Parser [Int]
pData = some (digit >>= return . read . singleton)

pInput :: Parser (Map2D Int)
pInput = pLines pData >>= return . mMap

-- Part 1

part1 :: (Map2D Int) -> Int
part1 _ = 1

-- Part 2

part2 :: (Map2D Int) -> Int
part2 _ = 2

-- Advent of Code 2025, day 4
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Data.Char
import Control.Applicative
import qualified Data.Map as M

import FunParser_old
import AoCTools_old

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

pData :: Parser [Int]
pData = do line <- label
           return (filter (\k -> line!!k == '@') [0 .. length line -1])

pInput :: Parser [Point]
pInput = do rows <- pLines pData
            return [(x,y) | x <- [0 .. length rows -1], y <- rows!!x]
-- Part 1

access :: [Point] -> Point -> Bool
access grid p = length (intersect grid (neighbours p)) < 4

part1 :: [Point] -> Int
part1 grid = length $ filter (access grid) grid

-- Part 2

remove :: [Point] -> Int
remove grid = if as == [] then 0 else length as + remove grid' where
  as = filter (access grid) grid
  grid' = grid \\ as

part2 :: [Point] -> Int
part2 = remove

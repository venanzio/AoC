-- Advent of Code 2024, day 9
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
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
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

diskBlocks :: [Int] -> [Int]
diskBlocks = dBlocks 0 where
  dBlocks n (f:e:map) = take f (repeat n) ++ take 3 (repeat (-1)) ++ dBlocks (n+1) map
  dBlocks n [f] = take f (repeat n)
  dBlocks _ [] = []


compact :: [Int] -> [Int]
compact = auxCompact . listLW where
  auxCompact blocks
    | isEmptyW blocks = []
    | h  >= 0 = h : auxCompact blocks1
    | isEmptyW blocks1 = []
    | otherwise = l : auxCompact blocks2
    where (h,blocks1) = extractW blocks
          (l,blocks2) = extractW (leftW blocks1)

pInput :: Parser [Int]
pInput = some (digit >>= \d -> return (read [d]))

-- Part 1

part1 :: [Int] -> Int
part1 _ = 1

-- Part 2

part2 :: [Int] -> Int
part2 _ = 2

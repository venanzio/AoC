-- Advent of Code 2024, day 2
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
  let reports = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 reports))
  putStrLn ("Part 2: " ++ show (part2 reports))

-- Parsing the input

pData :: Parser [Int]
pData = many natural

pInput :: Parser [[Int]]
pInput = pLines pData

-- Part 1

increasing :: [Int] -> Bool
increasing = allRel (<)

decreasing :: [Int] -> Bool
decreasing = allRel (>)

diff3 :: [Int] -> Bool
diff3 = allRel (\x y -> abs (x-y)<=3)

safe :: [Int] -> Bool
safe report = diff3 report && (increasing report || decreasing report)

part1 :: [[Int]] -> Int
part1 reports = length (filter safe reports)

-- Part 2

safeErr :: [Int] -> Bool
safeErr report = safe report || any safe (delOne report)

part2 :: [[Int]] -> Int
part2  reports = length (filter safeErr reports)

-- Advent of Code 2025, day 3
--  Venanzio Capretta

module Main where

import System.Environment
import Control.Applicative

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

pData :: Parser [Int]
pData = some (digit >>= \d -> return $ read [d])

pInput :: Parser [[Int]]
pInput = pLines pData

-- Solutions

jolt :: Int -> [Int] -> Int
jolt 0 _ = 0
jolt k b = x*10^(k-1) + jolt (k-1) (tail bt) where
  bi = take (length b - k + 1) b
  x = maximum bi
  (_,bt) = break (==x) b

part1 :: [[Int]] -> Int
part1 = sum . map (jolt 2)

part2 :: [[Int]] -> Int
part2 = sum . map (jolt 12)

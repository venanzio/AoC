-- Advent of Code 2025, day 3
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
pData = some (digit >>= \d -> return $ read [d])

pInput :: Parser [[Int]]
pInput = pLines pData

-- Part 1

joltage :: [Int] -> Int
joltage b = x * 10 + maximum (tail bt) where
  x = maximum (init b)
  (_,bt) = break (== x) b

part1 :: [[Int]] -> Int
part1 = sum . map joltage

-- Part 2

jolt :: Int -> [Int] -> Int
jolt 0 _ = 0
jolt n b = x*10^(n-1) + jolt (n-1) (tail bt) where
  bi = take (length b - n + 1) b
  x = maximum bi
  (_,bt) = break (==x) b


part2 :: [[Int]] -> Int
part2 = sum . map (jolt 12)

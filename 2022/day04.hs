-- Advent of Code 2022, day 4
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

type Range = (Int,Int)

pRange :: Parser Range
pRange = do start <- integer
            symbol "-"
            end <- integer
            return (start,end)

pData :: Parser (Range,Range)
pData = do first <- pRange
           symbol ","
           second <- pRange
           return (first,second)

pInput :: Parser [(Range,Range)]
pInput = pLines pData

-- Part 1

contained :: (Range,Range) -> Bool
contained ((s1,e1),(s2,e2)) = s1 >= s2 && e1<=e2

eitherContained :: (Range,Range) -> Bool
eitherContained (r1,r2) = contained (r1,r2) || contained (r2,r1)

part1 :: [(Range,Range)] -> Int
part1 = length . filter eitherContained

-- Part 2

overlap :: (Range,Range) -> Bool
overlap ((s1,e1),(s2,e2)) = not (e1<s2 || e2<s1)

part2 :: [(Range,Range)] -> Int
part2 = length . filter overlap


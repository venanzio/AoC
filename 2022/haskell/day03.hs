-- Advent of Code 2022, day 3
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


pData :: Parser (String,String)
pData = do rucksack <- word
           return (splitRucksack rucksack)

pInput :: Parser [String]
pInput = many word

-- Part 1

splitRucksack :: String -> (String,String)
splitRucksack rucksack = splitAt (length rucksack `div` 2) rucksack

value :: Char -> Int
value c = if isLower c then ord c - ord 'a' + 1
                       else ord c - ord 'A' + 27

ruckValue :: (String,String) -> Int
ruckValue (xs,ys) = value $ head $ intersect xs ys

part1 :: [String] -> Int
part1 = sum . map (ruckValue.splitRucksack)

-- Part 2

div3 :: [a] -> [(a,a,a)]
div3 (x:y:z:as) = (x,y,z):div3 as
div3 as = []

groupValue :: (String,String,String) -> Int
groupValue (x,y,z) = value $ head $ intersect (intersect x y) z

part2 :: [String] -> Int
part2 = sum . map groupValue . div3

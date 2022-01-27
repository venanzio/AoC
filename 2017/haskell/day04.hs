-- Advent of Code 2017, day 4

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
  putStrLn "Advent of Code 2017, day 4"
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type Phrase = [String]

pPhrase :: Parser Phrase
pPhrase = some word

pInput :: Parser [Phrase]
pInput = pLines pPhrase

-- Part 1

noDups :: (a->a->Bool) -> [a] -> Bool
noDups eq xs = length xs == length (nubBy eq xs)

countValid :: (a->a->Bool) -> [[a]] -> Int
countValid eq = length . filter (noDups eq)

part1 ::  [Phrase] -> Int
part1 = countValid (==)

-- Part 2

anagram :: String -> String -> Bool
anagram s1 s2 = sort s1 == sort s2

part2 ::  [Phrase] -> Int
part2 = countValid anagram

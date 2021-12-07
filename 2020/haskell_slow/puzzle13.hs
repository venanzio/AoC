-- Advent of Code 2020, day 13

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

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

pData :: Parser ()
pData = return ()

pInput :: Parser [()]
pInput = pLines pData

-- Part 1

part1 :: [()] -> Int
part1 _ = 1

-- Part 2

part2 :: [()] -> Int
part2 _ = 2

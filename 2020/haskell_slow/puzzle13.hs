-- Advent of Code 2020, day 13

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
  let (t,bs) = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 t bs))
  putStrLn ("Part 2: " ++ show (part2 t bs))

-- Parsing the input

pBus :: Parser (Maybe Int)
pBus = (natural >>= return.Just) <|> (symbol "x" >> return Nothing)

pBusses :: Parser [Int]
pBusses = (seqSep pBus ",") >>= return.filterJust

pInput :: Parser (Int,[Int])
pInput = do time <- natural
            busses <- pBusses
            return (time,busses)

-- Part 1

part1 :: Int -> [Int] -> Int
part1 t bs = t

-- Part 2

part2 :: Int -> [Int] -> Int
part2 _ _ = 2

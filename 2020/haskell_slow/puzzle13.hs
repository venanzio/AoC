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
  putStrLn (show (map (busTime t) bs))
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

-- for a given bus, return the next departure time after t
busTime :: Int -> Int -> Int
busTime t b = let r = t `rem` b
              in if r==0 then t else t+b-r

-- first bus to depart and time of departure
firstBus :: Int -> [Int] -> (Int,Int)
firstBus t bs = let (_,b,tb) = minimumF (busTime t) bs in (b,tb)

part1 :: Int -> [Int] -> Int
part1 t bs = let (b,t') = (firstBus t bs) in b * (t'-t)

-- Part 2

part2 :: Int -> [Int] -> Int
part2 _ _ = 2

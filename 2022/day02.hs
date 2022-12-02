-- Advent of Code 2022, day 2
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
pData = do opponent <- word
           you <- word
           return (opponent,you)

pInput :: Parser [(String,String)]
pInput = many pData

-- Part 1

data RPS = Rock | Paper | Scissors
  deriving Eq

aRPS :: String -> RPS
aRPS "A" = Rock
aRPS "B" = Paper
aRPS "C" = Scissors

xRPS :: String -> RPS
xRPS "X" = Rock
xRPS "Y" = Paper
xRPS "Z" = Scissors

axRPS :: (String,String) -> (RPS,RPS)
axRPS (a,x) = (aRPS a, xRPS x)

shapeScore :: RPS -> Int
shapeScore Rock     = 1
shapeScore Paper    = 2
shapeScore Scissors = 3

outcomeScore :: RPS -> RPS -> Int
outcomeScore Rock     Scissors = 0
outcomeScore Scissors Paper    = 0
outcomeScore Paper    Rock     = 0
outcomeScore a x = if a==x then 3 else 6

score :: (RPS,RPS) -> Int
score (a,x) = shapeScore x + outcomeScore a x

part1 :: [(String,String)] -> Int
part1 = sum . map (score.axRPS)

-- Part 2

winOver :: RPS -> RPS
winOver Rock = Paper
winOver Paper = Scissors
winOver Scissors = Rock

loseTo :: RPS -> RPS
loseTo Rock = Scissors
loseTo Paper = Rock
loseTo Scissors = Paper

choose :: RPS -> String -> RPS
choose a "X" = loseTo a
choose a "Y" = a
choose a "Z" = winOver a

chooseRPS :: (String,String) -> (RPS,RPS)
chooseRPS (a,x) = let a' = aRPS a in (a', choose a' x)

part2 :: [(String,String)] -> Int
part2 = sum . map (score.chooseRPS) 


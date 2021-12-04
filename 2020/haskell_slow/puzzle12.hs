-- Advent of Code 2020, day 12

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

-- Data types

type Action = (Char,Int)

type Ship = (Int,Int,Int)

-- Parsing the input

action :: Parser Char
action = char 'N' <|> char 'S'  <|> char 'E' <|> char 'W' <|>
         char 'L'  <|> char 'R'  <|> char 'F'

pData :: Parser Action
pData = do act <- action
           value <- natural
           return (act,value)

pInput :: Parser [Action]
pInput = pLines pData

-- Part 1

move :: Ship -> Action -> Ship
move (x,y,d) ('N',n) = (x,y+n,d)
move (x,y,d) ('S',n) = (x,y-n,d)
move (x,y,d) ('E',n) = (x+n,y,d)
move (x,y,d) ('W',n) = (x-n,y,d)
move (x,y,d) ('L',n) = (x,y, (d+n) `mod` 360)
move (x,y,d) ('R',n) = (x,y, (d-n) `mod` 360)
move (x,y,d) ('F',n) -- assume the direction is a multiple of 90
  | d==0                 = (x+n,y,d)
  | d==90  || d==(-270)  = (x,y+n,d)
  | d==180 || d ==(-180) = (x-n,y,d)
  | d==270 || d ==(-90)  = (x,y-n,d)
  | otherwise            = error ("wrong direction" ++ show d)

navigate :: Ship -> [Action] -> Ship
navigate = foldl move

mDistance :: Ship -> Int
mDistance (x,y,_) = abs x + abs y

part1 :: [Action] -> Int
part1 acts = mDistance $ navigate (0,0,0) acts

-- Part 2

part2 :: [Action] -> Int
part2 _ = 2

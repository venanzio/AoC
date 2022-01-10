-- Advent of Code 2021, day 2

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

pInput :: Parser [(String,Int)]
pInput = some (pPair word integer)

-- Part 1

move :: (Int,Int) -> (String,Int) -> (Int,Int)
move (pos,dep) ("forward",x) = (pos+x,dep)
move (pos,dep) ("down",x) = (pos,dep+x)
move (pos,dep) ("up",x) = (pos,dep-x)

part1 :: [(String,Int)] -> Int
part1 mvs = let (p,d) = foldl move (0,0) mvs in p*d

-- Part 2

move2 :: (Int,Int,Int) -> (String,Int) -> (Int,Int,Int)
move2 (pos,dep,aim) ("forward",x) = (pos+x,dep+aim*x,aim)
move2 (pos,dep,aim) ("down",x) = (pos,dep,aim+x)
move2 (pos,dep,aim) ("up",x) = (pos,dep,aim-x)


part2 ::  [(String,Int)] -> Int
part2 mvs =  let (p,d,a) = foldl move2 (0,0,0) mvs in p*d


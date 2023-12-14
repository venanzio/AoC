-- Advent of Code 2023, day 14
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
  input <- readFile fileName >>= return . lines
  let (round,cube) = pInput input
  putStrLn ("Round: " ++ show round)
  putStrLn ("Cube: " ++ show cube)
  putStrLn ("Part 1: " ++ show (part1 round cube))
  putStrLn ("Part 2: " ++ show (part2 round cube))

-- Parsing the input

type Position = (Int,Int)

pInput :: [String] -> ([Position],[Position])
pInput input = foldr addPos ([],[]) allPos where
  addPos (x,y) (round,cube) = case input!!y!!x of
    'O' -> ((x,y):round,cube)
    '#' -> (round,(x,y):cube)
    _   -> (round,cube)
  allPos = [(x,y) | y <- [0..length input - 1], x <- [0..length (input!!y) - 1]]

-- Part 1

part1 :: [Position] -> [Position] -> Int
part1 _ _ = 1

-- Part 2

part2 :: [Position] -> [Position] -> Int
part2 _ _ = 2

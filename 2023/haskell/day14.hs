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
      sEdge = length input
      eEdge = length (input!!0)
  putStrLn ("Part 1: " ++ show (part1 round cube sEdge))
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

slideNorth :: [Position] -> [Position] -> Position -> Position
slideNorth round cube (x,y) =
  (x, maximum (-1:[y0 | (x0,y0) <-round++cube, x0==x, y0<y]) + 1)

slideAll :: [Position] -> [Position] -> [Position] 
slideAll round cube = foldl (\rs p -> (slideNorth rs cube p) : rs) [] (sort round)
                          
part1 :: [Position] -> [Position] -> Int -> Int
part1 round cube edge = sum [edge - y | (_,y) <- slideAll round cube]

-- Part 2

slideWest :: [Position] -> [Position] -> Position -> Position
slideWest round cube (x,y) =
  (maximum (-1:[x0 | (x0,y0) <-round++cube, y0==y, x0<x]) + 1, y)

slideSouth :: Int -> [Position] -> [Position] -> Position -> Position
slideSouth sEdge round cube (x,y) =
  (x, minimum (sEdge:[y0 | (x0,y0) <-round++cube, x0==x, y0>x]) - 1)

slideEast :: Int -> [Position] -> [Position] -> Position -> Position
slideEast eEdge round cube (x,y) =
  (minimum (eEdge:[x0 | (x0,y0) <-round++cube, y0==y, x0>x]) - 1, y)


part2 :: [Position] -> [Position] -> Int
part2 _ _ = 2

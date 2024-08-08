-- Advent of Code 2017, day 3

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
  putStr("input number: ")
  input <- getLine
  let x = read input
  puzzle x

input = 289326 :: Int

puzzle :: Int -> IO ()
puzzle x = do
  putStrLn ("Advent of Code 2017, day 3")
  putStrLn ("Part 1: " ++ show (part1 x))
  putStrLn ("Part 2: " ++ show (part2 x))

-- Part 1

-- "inner" square, return n s.t. (2n-1)^2 < x <= (2n+1)^2
square_lt :: Int -> Int
square_lt x = sqlt 0
  where sqlt n = if x <= (2*n+1)^2 then n else sqlt (n+1)

-- coordinates of x around the frame of the square (2n-1)^2
-- each side is 2*n long
frame_tour :: Int -> Int -> (Int,Int)
frame_tour x n
  | x1<=2*n = (n,n-x1)    -- right side
  | x2<=2*n = (n-x2,-n)   -- top side
  | x3<=2*n = (-n,-n+x3)  -- left side
  | otherwise = (-n+x4,n) -- bottom side
  where x1 = x - (2*n-1)^2
        x2 = x1-2*n
        x3 = x2-2*n
        x4 = x3-2*n

-- coordinate of the square number x
square_coords :: Int -> (Int,Int)
square_coords x =
  if x == 1 then (0,0) else frame_tour x (square_lt x) 

-- Manhattan distance from (0,0)
manhattan :: (Int,Int) -> Int
manhattan (x,y) = abs x + abs y

part1 :: Int -> Int
part1 x = manhattan (square_coords x)

-- Part 2

type Grid = M.Map (Int,Int) Int

directions = [(u,v) | u <- [-1,0,1], v <- [-1,0,1], (u,v) /= (0,0)]

neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (i,j) = map (\(u,v)->(i+u,j+v)) directions

-- sum of entries in the neighbouring squares of the grid
grid_sum :: Grid -> (Int,Int) -> Int
grid_sum _ (0,0) = 1
grid_sum gr p = sum [gr M.! c | c <- neighbours p, c `M.member` gr]

-- fill in the grid progressively until finding a value larger than v
grid_gt :: Grid -> Int -> Int -> Int
grid_gt gr v x =
  let p = square_coords x
      s = grid_sum gr p              
  in if s > v then s else grid_gt (M.insert p s gr) v (x+1)

part2 :: Int -> Int
part2 v = grid_gt M.empty v 1

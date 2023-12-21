-- Advent of Code 2023, day 21
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
  input <- readFile fileName >>= return.lines
  let bxy = bounds input
      rs = rocks input
      s = sPoint bxy input
  putStrLn ("Part 1: " ++ show (part1 bxy rs s))
  putStrLn ("Part 2: " ++ show (part2 bxy rs s))

-- Parsing the input

bounds :: [String] -> Point
bounds input = (length (head input) - 1, length input - 1)

sPoint :: Point -> [String] -> Point
sPoint (bx,by) input = head (filter (\(x,y) -> input!!y!!x == 'S')
                                    [(x,y) | x <- [0..bx], y <- [0..by]])

rocks :: [String] -> [Point]
rocks = rockLines 0 where
  rockLines _ [] = []
  rockLines y (l:ls) = map (\x -> (x,y)) (rockLine l) ++ rockLines (y+1) ls

  rockLine = foldl (\xs (x,c) -> if c=='#' then x:xs else xs) [] . zip [0..] 

-- Part 1

directions = [(0,-1),(0,1),(-1,0),(1,0)]

move :: Point -> Point -> Point
move (x,y) (dx,dy) = (x+dx,y+dy)

inBounds :: Point -> Point -> Bool
inBounds (bx,by) (x,y) = x>=0 && x <=bx && y>=0 && y<=by

step :: Point -> [Point] -> Point -> [Point]
step bxy rocks p = (filter (inBounds bxy) $ map (move p) directions) \\ rocks

steps :: Point -> [Point] -> [Point] -> [Point]
steps bxy rocks ps = nub $ concat $ map (step bxy rocks) ps

part1 :: Point -> [Point] -> Point -> Int
part1 bxy rocks s = length $ nIter (steps bxy rocks) 64 [s] 

-- Part 2

noRock :: Point -> [Point] -> Point -> Bool
noRock (bx,by) rocks (x,y) = not $ (x `mod` (bx+1), y `mod` (by+1)) `elem` rocks

step2 :: Point -> [Point] -> Point -> [Point]
step2 bxy rocks p = filter (noRock bxy rocks) $ map (move p) directions

steps2 :: Point -> [Point] -> [Point] -> [Point]
steps2 bxy rocks ps = nub $ concat $ map (step2 bxy rocks) ps

part2 :: Point -> [Point] -> Point -> Int
part2 bxy rocks s = length $ nIter (steps2 bxy rocks) 100 [s] 



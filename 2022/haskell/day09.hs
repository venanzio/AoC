-- Advent of Code 2022, day 9
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

data Direction = L | R | U | D
  deriving (Eq,Show)

pData :: Parser (Direction,Int)
pData = do dir <- (symbol "L" >> return L) <|>
                    (symbol "R" >> return R) <|>
                      (symbol "U" >> return U) <|>
                        (symbol "D" >> return D)
           steps <- natural
           return (dir,steps)
           
pInput :: Parser [(Direction,Int)]
pInput = many pData

-- Part 1

type Coords = (Int,Int)

move :: Coords -> Direction -> Coords
move (x,y) L = (x-1,y)
move (x,y) R = (x+1,y)
move (x,y) U = (x,y+1)
move (x,y) D = (x,y-1)

dist :: Coords -> Coords -> Int
dist (x1,y1) (x2,y2) = max (abs (x2-x1)) (abs (y2-y1))

follow :: Coords -> Coords -> Coords
follow c1@(x1,y1) c2@(x2,y2) =
  if dist c1 c2 > 1 then (x1 + signum (x2-x1), y1 + signum (y2-y1)) else c1

tailPos :: Coords -> Coords -> [(Direction,Int)] -> [Coords]
tailPos hd tl [] = []
tailPos hd tl ((_,0):steps) = tailPos hd tl steps
tailPos hd tl ((d,n):steps) =
  let hd' = move hd d
      tl' = follow tl hd'
      steps' = (d,n-1):steps
  in if tl' == tl then tailPos hd' tl' steps' else tl' : tailPos hd' tl' steps'

tailPositions :: Coords -> Coords -> [(Direction,Int)] -> [Coords]
tailPositions hd tl steps = tl : tailPos hd tl steps

part1 :: [(Direction,Int)] -> Int
part1 = length . nub . tailPositions (0,0) (0,0)

-- Part 2

ropeMove :: [Coords] -> Direction -> [Coords]
ropeMove (hd:knots) d = let hd' = move hd d in ropeFollow hd' knots

ropeFollow :: Coords -> [Coords] -> [Coords]
ropeFollow k0 [] = [k0]
ropeFollow k0 (k1:knots) = let k1' = follow k1 k0 in k0 : ropeFollow k1' knots

ropePos :: [Coords] -> [(Direction,Int)] -> [Coords]
ropePos rope [] = []
ropePos rope ((_,0):steps) = ropePos rope steps
ropePos rope ((d,n):steps) =
  let tl = last rope
      rope' = ropeMove rope d
      tl' = last rope'
      steps' = (d,n-1):steps
  in if tl' == tl then ropePos rope' steps' else tl' : ropePos rope' steps'

ropePositions :: [Coords] -> [(Direction,Int)] -> [Coords]
ropePositions rope steps = last rope : ropePos rope steps

part2 :: [(Direction,Int)] -> Int
part2 = length . nub . ropePositions (take 10 (repeat (0,0)))

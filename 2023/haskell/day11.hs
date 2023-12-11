-- Advent of Code 2023, day 0

--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  image <- readFile fileName >>= return . lines
  let gs = galaxies image
      expRs = emptyRs image
      expCs = emptyCs image
  putStrLn ("Part 1: " ++ show (part1 expRs expCs gs))
  putStrLn ("Part 2: " ++ show (part2 expRs expCs gs))

-- Expanding the image

type Image = [String]
type Position = (Int,Int)

galaxies :: Image -> [Position]
galaxies image = [(x,y) | y <- [0..length image - 1], x  <- [0..length (image!!y) - 1],
                          image!!y!!x == '#']

distance :: Position -> Position -> Int
distance (x0,y0) (x1,y1) = abs (x1-x0) + abs (y1-y0)

emptyRs :: Image -> [Int]
emptyRs = findIndices (all (=='.'))

emptyCs :: Image -> [Int]
emptyCs = emptyRs . transpose

-- expansion of a coordinate (given expansion rate and coordinates to expand)
expandC :: Int -> [Int] -> Int -> Int
expandC expRate expXs x =
  x + (expRate-1)*(length $ filter (<x) expXs)

-- expanding a position given expansion rate and rows and columns to expand
expand :: Int -> [Int] -> [Int] -> Position -> Position
expand expRate expRs expCs (x,y) =
  (expandC expRate expCs x, expandC expRate expRs y)

sumDistances :: [Position] -> Int
sumDistances gs = sum [distance g0 g1 | (g0,g1) <- allPairs gs]

-- Part 1

part1 :: [Int] -> [Int] -> [Position] -> Int
part1 expRs expCs gs = sumDistances $ map (expand 2 expRs expCs) gs

-- Part 2

part2 :: [Int] -> [Int] -> [Position] -> Int
part2 expRs expCs gs =  sumDistances $ map (expand 1000000 expRs expCs) gs


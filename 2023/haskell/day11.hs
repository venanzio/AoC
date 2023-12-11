-- Advent of Code 2023, day 0

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
  image <- readFile fileName >>= return . lines
  let gs = galaxies image
      expRs = emptyRs image
      expCs = emptyCs image
  putStrLn ("Part 1: " ++ show (part1 expRs expCs gs))
  putStrLn ("Part 2: " ++ show (part2 expRs expCs gs))

-- Expanding the image

type Image = [String]

expandR :: Image -> Image
expandR [] = []
expandR (r:rs) = if all (=='.') r then r:r:expandR rs else r:expandR rs

expand :: Image -> Image
expand = transpose . expandR . transpose . expandR

-- Part 1

type Position = (Int,Int)

galaxies :: Image -> [Position]
galaxies image = [(x,y) | y <- [0..length image - 1], x  <- [0..length (image!!y) - 1],
                          image!!y!!x == '#']

distance :: Position -> Position -> Int
distance (x0,y0) (x1,y1) = abs (x1-x0) + abs (y1-y0)

part1 :: [Int] -> [Int] -> [Position] -> Int
part1 expRs expCs gs =
  let gsE = map (galaxyDist 2 expRs expCs) gs
  in sum [distance g0 g1 | (g0,g1) <- allPairs gsE]

-- Part 2

emptyRs :: Image -> [Int]
emptyRs = findIndices (all (=='.'))

emptyCs :: Image -> [Int]
emptyCs = emptyRs . transpose


expandD :: Int -> [Int] -> Int -> Int
expandD expRate expRs x =
  x + (expRate-1)*(length $ filter (<x) expRs)

galaxyDist :: Int -> [Int] -> [Int] -> Position -> Position
galaxyDist expRate expRs expCs (x,y) =
  (expandD expRate expCs x, expandD expRate expRs y)


part2 :: [Int] -> [Int] -> [Position] -> Int
part2 expRs expCs gs =
  let gsE = map (galaxyDist 1000000 expRs expCs) gs
  in sum [distance g0 g1 | (g0,g1) <- allPairs gsE]

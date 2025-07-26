-- Advent of Code 2024, day 8
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
-- import Data.Char
-- import Control.Applicative
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
  let lns = lines input
      maxX = length (lns!!0) - 1
      maxY = length lns - 1
      antennas = stringsMap (lines input)
  putStrLn (show maxX ++ " - " ++ show maxY)
  putStrLn (show (nub $ M.elems antennas))
  putStrLn ("Part 1: " ++ show (part1 maxX maxY antennas))
  putStrLn ("Part 2: " ++ show (part2 maxX maxY antennas))

-- Part 1

antinodes :: Point -> Point -> [Point]
antinodes p0 p1 = let d = pDist p0 p1 in [pMove p0 (pNeg d), pMove p1 d]

allAntinodes :: [Point] -> [Point]
allAntinodes ps = concat [antinodes p0 p1 | (p0,p1) <- allPairs ps]

part1 :: Int -> Int -> Map2D Char -> Int
part1 maxX maxY antennas = length $
  filter (pInside (0,0) (maxX,maxY)) $ nub $ concat
    [allAntinodes (mFind freq antennas) | freq <- (nub $ M.elems antennas)]
  

-- Part 2

antiLine :: Int -> Int -> Point -> Point -> [Point]
antiLine maxX maxY p0 p1 =
  let (dx,dy) = pDist p0 p1
      g = gcd dx dy
      vx = dx `div` g
      vy = dy `div` g
  in (takeWhile (pInside (0,0) (maxX,maxY)) [pMove p0 (i*vx,i*vy) | i <- [0..]]) ++
     (takeWhile (pInside (0,0) (maxX,maxY)) [pMove p0 (i*vx,i*vy) | i <- [-1..]])

allAntinodes2 :: Int -> Int -> [Point] -> [Point]
allAntinodes2 maxX maxY ps =
  concat [antiLine maxX maxY p0 p1 | (p0,p1) <- allPairs ps]

part2 :: Int -> Int -> Map2D Char -> Int
part2 maxX maxY antennas = 2

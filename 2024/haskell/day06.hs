-- Advent of Code 2024, day 6
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
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
  let ls = lines input
      maxX = length (ls!!1) - 1
      maxY = length ls - 1
      map = mMap ls
  putStrLn ("Part 1: " ++ show (part1 maxX maxY map))
  putStrLn ("Part 2: " ++ show (part2 maxX maxY map))

-- Part 1

patrol :: Int -> Int -> Map2D Char -> Point -> Point -> Map2D Char
patrol maxX maxY map p d
  | not (pInside (0,0) (maxX,maxY) p) = map
  | M.lookup p' map == Just '#' = patrol maxX maxY map p (dRTurn d)
  | otherwise = patrol maxX maxY map' p' d
  where p' = pMove p d
        map' = M.insert p 'X' map

part1 :: Int -> Int -> Map2D Char -> Int
part1 maxX maxY map =
  length $ mFind 'X' (patrol maxX maxY map (head $ mFind '^' map) dUp)

-- Part 2

part2 :: Int -> Int -> Map2D Char -> Int
part2 maxX maxY map = 2

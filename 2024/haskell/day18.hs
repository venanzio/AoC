-- Advent of Code 2024, day 18
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
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser Point
pData = do x <- natural
           symbol ","
           y <- natural
           return (x,y)

pInput :: Parser [Point]
pInput = pLines pData

-- Part 1

spaceGraph :: Int -> Int -> [Point] -> Graph Point
spaceGraph maxX maxY corrupt =
  M.fromList [((i,j), [(m,1) | m <- neighboursHV (i,j), not (m `elem` corrupt)]) |
                           i <- [0..maxX], j <- [0..maxY]]
          
part1 :: [Point] -> Int
part1 xs = dijkstra (spaceGraph 6 6 (take 12 xs)) (0,0) (6,6)

-- Part 2

part2 :: [Point] -> Int
part2 _ = 2

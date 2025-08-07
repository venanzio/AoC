-- Advent of Code 2024, day 18
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
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

maxX = 70
maxY = 70

drops = 1024

spaceGraph :: [Point] -> Graph Point
spaceGraph corrupt =
  M.fromList [((i,j), [(m,1) | m <- neighboursHV (i,j), not (m `elem` corrupt)]) |
                           i <- [0..maxX], j <- [0..maxY]]

part1 :: [Point] -> Int
part1 xs = dijkstra (spaceGraph (take drops xs)) (0,0) (maxX,maxY)

-- Part 2

-- Idea: chose a path (from part 1), recompute when it's blocked

blocked :: Eq a => [a] -> [a] -> ([a],[a])
blocked path ps = span (not . (`elem` path)) ps

cutOff :: [Point] -> [Point] -> Point
cutOff qs ps = if paths == [] then head qs else cutOff (p:pre++qs) post
  where paths = snd $ dijkstraPaths (spaceGraph qs) (0,0) (maxX,maxY)
        (pre,p:post) = blocked (head paths) ps

part2 :: [Point] -> Point
part2 xs = let (pre,post) = splitAt drops xs in cutOff pre post


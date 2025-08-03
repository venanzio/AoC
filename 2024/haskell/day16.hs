-- Advent of Code 2024, day 16
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
  let maze = stringsMap (lines input)
  putStrLn ("Part 1: " ++ show (part1 maze))
  putStrLn ("Part 2: " ++ show (part2 maze))

-- Part 1

mazeGraph :: Map2D Char -> Graph (Point,Direction)
mazeGraph maze = M.fromList $ [(n,step n) | i <- [1..maxX], j <- [1..maxY],
                                            n <- node (i,j)]
  where (maxX,maxY) = fst $ M.findMax maze
        pE = head $ mFind 'E' maze
        node p = (pE,(0,0)) : case M.lookup p maze of
          Just '#' -> []
          _ -> map (\d -> (p,d)) directionsHV
        step (p,d)
          | p == pE && d == (0,0) = []
          | p == pE               = [((pE,(0,0)),0)]
          | M.lookup (pMove p d) maze /= Just '#' = ((pMove p d,d),1) : turns
          | otherwise = turns
          where turns = [((p,dRTurn d),1000),((p,dLTurn d),1000)]

  
part1 :: Map2D Char -> Int
part1 maze = dijkstra (mazeGraph maze)
                      (head $ mFind 'S' maze, dRight)
                      (head $ mFind 'E' maze, (0,0))

-- Part 2

part2 :: Map2D Char -> Int
part2 _ = 2

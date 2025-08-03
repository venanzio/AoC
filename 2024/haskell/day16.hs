-- Advent of Code 2024, day 16
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
  let maze = stringsMap (lines input)
  putStrLn (showMap id maze)
  putStrLn ("Part 1: " ++ show (part1 maze))
  putStrLn ("Part 2: " ++ show (part2 maze))

-- Part 1

mazeGraph :: Map2D Char -> Graph (Point,Direction)
mazeGraph maze = M.fromList [(n,step n) | i <- [1..maxX], j <- [1..maxY],
                                          n <- node (i,j)]
  where (maxX,maxY) = fst $ M.findMax maze
        node p = case M.lookup p maze of
          Just '#' -> []
          _ -> map (\d -> (p,d)) directionsHV
        step (p,d)   = if M.lookup p maze /= Just '#'
                         then [((pMove p d,d),1)]
                         else []
                       ++ [((p,dRTurn d),1000),
                           ((p,dLTurn d),1000)]

  
part1 :: Map2D Char -> Int
part1 _ = 1

-- Part 2

part2 :: Map2D Char -> Int
part2 _ = 2

-- Advent of Code 2023, day 17
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
  let maxX = length (input!!0)-1
      maxY = length input - 1
      graph = pGraph input
      graph2 = pGraph2 input
  -- putStrLn ("Part 1: " ++ show (part1 graph maxX maxY))
  putStrLn ("Part 2: " ++ show (part2 graph2 maxX maxY))

-- Parsing the input

newDir :: (Int,Int) -> [(Int,Int)]
newDir (0,0) = [(0,-1),(0,1),(-1,0),(1,0)]
newDir (dx,0) = (0,-1):(0,1):
  if abs dx == 3 then []
                 else [(dx+(signum dx),0)]
newDir (0,dy) = (-1,0):(1,0):
  if abs dy == 3 then []
                 else [(0,dy+(signum dy))]
newDir _ = []

move :: Int -> Int -> (Int,Int) -> (Int,Int)
move x y (dx,dy) = (x+signum dx,y+signum dy)

pGraph :: [String] -> Graph (Int,Int,Int,Int)
pGraph input =
  let maxX = length (input!!0)-1
      maxY = length input - 1
      moves x y dx dy = ((x,y,3,3),0):
                        [((x0,y0,dx0,dy0),read [input!!y0!!x0])
                        | (dx0,dy0) <- (newDir (dx,dy))
                        , (x0,y0) <- [move x y (dx0,dy0)]
                        , 0<=x0, x0<=maxX, 0<=y0, y0<=maxY
                        ]
  in M.fromList $ [((x,y,dx,dy), moves x y dx dy)
                  | x <- [0..maxX]
                  , y <- [0..maxY]
                  , (dx,dy) <- (3,3):[(dx,0) | dx <- [-3..3]] ++ [(0,dy) | dy <- [-3..3]]
                  ]

-- Part 1

part1 :: Graph (Int,Int,Int,Int) -> Int -> Int -> Int
part1 graph maxX maxY = dijkstra graph (0,0,0,0) (maxX,maxY,3,3)

-- Part 2

newDir2 :: (Int,Int) -> [(Int,Int)]
newDir2 (0,0) = [(0,-1),(0,1),(-1,0),(1,0)]
newDir2 (dx,0) | abs dx < 4  = [(dx+(signum dx),0)]
               | abs dx < 10 = [(0,-1),(0,1),(dx+(signum dx),0)]
               | otherwise   = [(0,-1),(0,1)]
newDir2 (0,dy) | abs dy < 4  = [(0,dy+(signum dy))]
               | abs dy < 10 = [(-1,0),(1,0),(0,dy+(signum dy))]
               | otherwise   = [(-1,0),(1,0)]
newDir2 _ = []

pGraph2 :: [String] -> Graph (Int,Int,Int,Int)
pGraph2 input =
  let maxX = length (input!!0)-1
      maxY = length input - 1
      moves x y 1 1 = []
      moves x y dx dy =
        (if abs (dx+dy)>=4 then [((x,y,1,1),0)] else []) ++
        [((x0,y0,dx0,dy0),read [input!!y0!!x0])
        | (dx0,dy0) <- (newDir2 (dx,dy))
        , (x0,y0) <- [move x y (dx0,dy0)]
        , 0<=x0, x0<=maxX, 0<=y0, y0<=maxY
        ]
  in M.fromList $
       [((x,y,dx,dy), moves x y dx dy)
       | x <- [0..maxX]
       , y <- [0..maxY]
       , (dx,dy) <- (1,1):[(dx,0) | dx <- [-10..10]] ++ [(0,dy) | dy <- [-10..10]]
       ]



part2 :: Graph (Int,Int,Int,Int) -> Int -> Int -> Int
part2 graph maxX maxY = dijkstra graph (0,0,0,0) (maxX,maxY,1,1)


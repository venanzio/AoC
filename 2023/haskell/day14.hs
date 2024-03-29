-- Advent of Code 2023, day 14
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
  input <- readFile fileName >>= return . lines
  let sEdge = length input
      eEdge = length (input!!0)
      (cube,round) = pInput input
  putStrLn ("Part 1: " ++ show (part1 sEdge cube round)) --test 136; 110821
  putStrLn ("Part 2: " ++ show (part2 sEdge eEdge cube round))  -- test:64; 83516 

-- Parsing the input

type Position = (Int,Int)

pInput :: [String] -> ([Position],[Position])
pInput input = foldr addPos ([],[]) allPos where
  addPos (x,y) (cube,round) = case input!!y!!x of
    'O' -> (cube,(x,y):round)
    '#' -> ((x,y):cube,round)
    _   -> (cube,round)
  allPos = [(x,y) | y <- [0..length input - 1], x <- [0..length (input!!y) - 1]]

showRocks :: Int -> Int -> [Position] -> [Position] -> String
showRocks sEdge eEdge cube round = unlines
  [[posRock (x,y) | x <- [0 .. eEdge -1]]
  | y <- [0 .. sEdge - 1]]
  where posRock pos =
          if pos `elem` cube then '#'
            else if pos `elem` round then 'O'
                   else '.'
                        
-- Part 1

slideNorth :: [Position] -> [Position] -> Position -> Position
slideNorth round cube (x,y) =
  (x, maximum (-1:[y0 | (x0,y0) <-round++cube, x0==x, y0<y]) + 1)

slideNAll :: [Position] -> [Position] -> [Position] 
slideNAll cube round = foldl (\rs p -> (slideNorth rs cube p) : rs) [] (sort round)

load :: Int -> [Position] -> [Position] -> Int
load sEdge cube round = sum [sEdge - y | (_,y) <- round]
                       
part1 ::  Int -> [Position] -> [Position] -> Int
part1 sEdge cube round = load sEdge cube (slideNAll cube round)

-- Part 2

slideWest :: [Position] -> [Position] -> Position -> Position
slideWest cube round (x,y) =
  (maximum (-1:[x0 | (x0,y0) <-cube++round, y0==y, x0<x]) + 1, y)

slideWAll :: [Position] -> [Position] -> [Position] 
slideWAll cube round = foldl (\rs p -> (slideWest cube rs p) : rs) []
             (sortOn (\(x,y) -> (y,x)) round)

slideSouth :: Int -> [Position] -> [Position] -> Position -> Position
slideSouth sEdge cube round (x,y) =
  (x, minimum (sEdge:[y0 | (x0,y0) <-cube++round, x0==x, y0>y]) - 1)

slideSAll :: Int -> [Position] -> [Position] -> [Position] 
slideSAll sEdge cube round = foldl (\rs p -> (slideSouth sEdge cube rs p) : rs) []
             (reverse $ sort round)

slideEast :: Int -> [Position] -> [Position] -> Position -> Position
slideEast eEdge cube round (x,y) =
  (minimum (eEdge:[x0 | (x0,y0) <-cube++round, y0==y, x0>x]) - 1, y)

slideEAll :: Int -> [Position] -> [Position] -> [Position] 
slideEAll eEdge cube round = foldl (\rs p -> (slideEast eEdge cube rs p) : rs) []
             (reverse $ sortOn (\(x,y) -> (y,x)) round)

spinCycle :: Int -> Int -> [Position] -> [Position] -> [Position]
spinCycle sEdge eEdge cube = sort .
  slideEAll eEdge cube . slideSAll sEdge cube . slideWAll cube . slideNAll cube

part2 :: Int -> Int -> [Position] -> [Position] -> Int
part2 sEdge eEdge cube round =
  load sEdge cube $ loopf (spinCycle sEdge eEdge cube) round 1000000000



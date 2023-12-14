-- Advent of Code 2023, day 14
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
  input <- readFile fileName >>= return . lines
  let (round,cube) = pInput input
      sEdge = length input
      eEdge = length (input!!0)
      roundN = slideNAll cube round
      roundW = slideWAll cube round
  putStrLn (showRocks sEdge eEdge cube roundW)
  putStrLn ("Part 1: " ++ show (part1 round cube sEdge))
  putStrLn ("Part 2: " ++ show (part2 round cube sEdge eEdge))

-- Parsing the input

type Position = (Int,Int)

showRocks :: Int -> Int -> [Position] -> [Position] -> String
showRocks sEdge eEdge cube round = unlines
  [[posRock (x,y) | x <- [0 .. eEdge -1]]
  | y <- [0 .. sEdge - 1]]
  where posRock pos =
          if pos `elem` cube then '#'
            else if pos `elem` round then 'O'
                   else '.'
    
pInput :: [String] -> ([Position],[Position])
pInput input = foldr addPos ([],[]) allPos where
  addPos (x,y) (round,cube) = case input!!y!!x of
    'O' -> ((x,y):round,cube)
    '#' -> (round,(x,y):cube)
    _   -> (round,cube)
  allPos = [(x,y) | y <- [0..length input - 1], x <- [0..length (input!!y) - 1]]

-- Part 1

slideNorth :: [Position] -> [Position] -> Position -> Position
slideNorth round cube (x,y) =
  (x, maximum (-1:[y0 | (x0,y0) <-round++cube, x0==x, y0<y]) + 1)

slideNAll :: [Position] -> [Position] -> [Position] 
slideNAll cube round = foldl (\rs p -> (slideNorth rs cube p) : rs) [] (sort round)

load :: [Position] -> [Position] -> Int -> Int
load round cube edge = sum [edge - y | (_,y) <- slideNAll cube round]
                       
part1 :: [Position] -> [Position] -> Int -> Int
part1 = load

-- Part 2

slideWest :: [Position] -> [Position] -> Position -> Position
slideWest cube round (x,y) =
  (maximum (-1:[x0 | (x0,y0) <-cube++round, y0==y, x0<x]) + 1, y)

slideWAll :: [Position] -> [Position] -> [Position] 
slideWAll cube round = foldl (\rs p -> (slideWest rs cube p) : rs) []
             (sortOn (\(x,y) -> (y,x)) cube)

slideSouth :: Int -> [Position] -> [Position] -> Position -> Position
slideSouth sEdge cube round (x,y) =
  (x, minimum (sEdge:[y0 | (x0,y0) <-cube++round, x0==x, y0>x]) - 1)

slideSAll :: Int -> [Position] -> [Position] -> [Position] 
slideSAll sEdge cube round = foldl (\rs p -> (slideSouth sEdge rs cube p) : rs) []
             (reverse $ sort cube)

slideEast :: Int -> [Position] -> [Position] -> Position -> Position
slideEast eEdge cube round (x,y) =
  (minimum (eEdge:[x0 | (x0,y0) <-cube++round, y0==y, x0>x]) - 1, y)

slideEAll :: Int -> [Position] -> [Position] -> [Position] 
slideEAll eEdge cube round = foldl (\rs p -> (slideEast eEdge rs cube p) : rs) []
             (reverse $ sortOn (\(x,y) -> (y,x)) cube)

spinCycle :: Int -> Int -> [Position] -> [Position] -> [Position]
spinCycle sEdge eEdge cube =
  slideEAll eEdge cube . slideSAll sEdge cube . slideWAll cube . slideNAll cube

stableCycle :: Int -> Int -> [Position] -> [Position] -> [Position]
stableCycle sEdge eEdge cube round =
  let cRound = spinCycle sEdge eEdge cube round
  in if cRound == round then round else stableCycle sEdge eEdge cube cRound

part2 :: [Position] -> [Position] -> Int -> Int -> Int
part2 round cube sEdge eEdge = load (stableCycle sEdge eEdge cube round) cube sEdge

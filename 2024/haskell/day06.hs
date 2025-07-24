-- Advent of Code 2024, day 6
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
  let ls = lines input
      maxX = length (ls!!1) - 1
      maxY = length ls - 1
      map = stringsMap ls
      p0 = start map
  putStrLn ("Part 1: " ++ show (part1 maxX maxY map))
  putStrLn ("Part 2: " ++ show (part2 maxX maxY map))

-- Part 1

start :: Map2D Char -> Point
start map = head $ mFind '^' map

patrol :: Int -> Int -> Map2D Char -> Point -> Point -> Map2D Char
patrol maxX maxY map p d
  | not (pInside (0,0) (maxX,maxY) p) = map
  | M.lookup p' map == Just '#' = patrol maxX maxY map p (dRTurn d)
  | otherwise = patrol maxX maxY map' p' d
  where p' = pMove p d
        map' = M.insert p 'X' map

part1 :: Int -> Int -> Map2D Char -> Int
part1 maxX maxY map =
  length $ mFind 'X' (patrol maxX maxY map (start map) dUp)

-- Part 2

-- to check a loop, use a map of visited points with directions
loop :: Int -> Int -> Map2D Char -> Point -> Point -> Map2D [Point] ->  Bool
loop maxX maxY map p d visited
  | d `elem` pVis = True
  | not (pInside (0,0) (maxX,maxY) p) = False 
  | M.lookup p' map == Just '#' = loop maxX maxY map p (dRTurn d) visited
  | otherwise = loop maxX maxY map p' d visited'
  where pVis = case M.lookup p visited of
                 Nothing -> []
                 Just ds -> ds
        visited' = M.insert p (d:pVis) visited 
        p' = pMove p d

part2 :: Int -> Int -> Map2D Char -> Int
part2 maxX maxY map =
  let p0 = start map
      trace = delete p0 (mFind 'X' (patrol maxX maxY map (start map) dUp))
      loops = filter (\p -> loop maxX maxY (M.insert p '#' map) p0 dUp M.empty) trace
  in length loops


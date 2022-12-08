-- Advent of Code 2022, day 8
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
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type Patch = M.Map (Int,Int) Int

pRow :: Parser [Int]
pRow = many (digit >>= \c -> return $ read [c])

pPatch :: Parser [[Int]]
pPatch = pLines pRow

pInput :: Parser Patch
pInput = pPatch >>= return . mMap

-- Part 1

type Direction = (Int,Int)

left :: Direction
left = (-1,0)

right :: Direction
right = (1,0)

up :: Direction
up = (0,-1)

down :: Direction
down = (0,1)


stepDir :: (Int,Int) -> Direction -> (Int,Int)
stepDir (i,j) (x,y) = (i+x,j+y)

elemsDir :: (Int,Int) -> Direction -> Patch -> [Int]
elemsDir pos dir patch =
  let pos' = stepDir pos dir
  in case patch M.!? pos' of
       Nothing -> []
       Just x -> x : elemsDir pos' dir patch

dirVisible :: (Int,Int) -> Direction -> Patch -> Bool
dirVisible pos dir patch = all (< patch M.! pos) (elemsDir pos dir patch)

visible :: (Int,Int) -> Patch -> Bool
visible pos patch = dirVisible pos left  patch ||
                    dirVisible pos right patch ||
                    dirVisible pos up    patch ||
                    dirVisible pos down  patch

countVis :: Patch -> Int
countVis patch = M.foldrWithKey (\pos h -> if visible pos patch then (+1) else id) 0 patch

part1 :: Patch -> Int
part1 = countVis

-- Part 2

viewDist :: (Int,Int) -> Direction -> Patch -> Int
viewDist pos dir patch = vdFrom (stepDir pos dir)
  where vdFrom p = case patch M.!? p of
          Nothing -> 0
          Just h -> if h >= patch M.! pos then 1 else 1 + vdFrom (stepDir p dir)

scenicScore :: (Int,Int) -> Patch -> Int
scenicScore pos patch = viewDist pos left patch *
                        viewDist pos right patch *
                        viewDist pos up patch *
                        viewDist pos down patch

part2 :: Patch -> Int
part2 patch = maximum (map (\pos -> scenicScore pos patch) (M.keys patch))

-- Advent of Code 2021, day 5

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

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

type Coords = (Int,Int)

type Floor = M.Map Coords Int

point :: Floor -> Coords -> Int
point f c = f M.! c

pData :: Parser (Coords,Coords)
pData = do x0 <- natural
           symbol ","
           y0 <- natural
           symbol "->"
           x1 <- natural
           symbol ","
           y1 <- natural
           return ((x0,y0),(x1,y1))
           
pInput :: Parser [(Coords,Coords)]
pInput = pLines pData

-- Part 1

initFloor :: Floor
initFloor = M.empty

range :: Int -> Int -> [Int]
range n m = if n<=m then [n..m] else [n,n-1..m]

ovLine :: Coords -> Coords -> [Coords]
ovLine (x0,y0) (x1,y1)
  | x0==x1 = [(x0,y) | y <- range y0 y1]
  | y0==y1 = [(x,y0) | x <- range x0 x1]
  | otherwise = []

mark :: Floor -> Coords -> Floor
mark f c = case M.lookup c f of
  Nothing -> M.insert c 1 f
  Just n  -> M.insert c (n+1) f

markLine :: Floor -> (Coords,Coords) -> Floor
markLine f (c0,c1) = foldl mark f (ovLine c0 c1)

markAll :: Floor -> [(Coords,Coords)] -> Floor
markAll = foldl markLine

countOverlap :: Floor -> Int
countOverlap = length . filter (>=2) . M.elems

part1 :: [(Coords,Coords)] -> Int
part1 coords = countOverlap (markAll initFloor coords)

-- Part 2

ventLine :: Coords -> Coords -> [Coords]
ventLine (x0,y0) (x1,y1)
  | x0==x1 = [(x0,y) | y <- range y0 y1]
  | y0==y1 = [(x,y0) | x <- range x0 x1]
  | otherwise = zip (range x0 x1) (range y0 y1)


markLine' :: Floor -> (Coords,Coords) -> Floor
markLine' f (c0,c1) = foldl mark f (ventLine c0 c1)

markAll' :: Floor -> [(Coords,Coords)] -> Floor
markAll' = foldl markLine'

part2 :: [(Coords,Coords)] -> Int
part2 coords = countOverlap (markAll' initFloor coords)

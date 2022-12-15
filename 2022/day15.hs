-- Advent of Code 2022, day 15
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

data Position = Sensor | Beacon
  deriving (Show,Eq)

type Coords = (Int,Int)

type Grid = M.Map Coords Coords

pRow :: Parser (Coords,Coords)
pRow = do symbol "Sensor at x="
          sx <- integer
          symbol ", y="
          sy <- integer
          symbol ": closest beacon is at x="
          bx <- integer
          symbol ", y="
          by <- integer
          return ((sx,sy),(bx,by))

pInput :: Parser Grid
pInput = do sbs <- pLines pRow
            return (M.fromList sbs)
-- Part 1

mDist :: Coords -> Coords -> Int
mDist (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)

type Range = [(Int,Int)]

rUnion :: Range -> Range -> Range
rUnion [] r = r
rUnion r [] = r
rUnion ((l0,h0):r0) ((l1,h1):r1)
  | h0 < l1 = (l0,h0) : rUnion r0 ((l1,h1):r1)
  | h1 < l0 = (l1,h1) : rUnion ((l0,h0):r0) r1
  | otherwise = rUnion ((min l0 l1, max h0 h1):r0) r1

rsUnion :: [Range] -> Range
rsUnion = foldl rUnion []

rLength :: Range -> Int
rLength = sum . map (\(l,h) -> h-l+1)

range :: Int -> Int -> Range
range x0 dist = if dist>0 then [(x0-dist, x0+dist)] else []

noNegR :: Range -> Range
noNegR = filter (\(l,h) -> l<=h)

noPoint :: Range -> Int -> Range
noPoint [] _ = []
noPoint ((l,h):r) x
  | x<l = (l,h) : r
  | x==l = noNegR $ (l+1,h) : r
  | x<h = noNegR $ (l,x-1) : (x+1,h) : r
  | x>h = (l,h) : noPoint r x

noPoints :: Range -> [Int] -> Range
noPoints = foldl noPoint

noBeacon :: Coords -> Coords -> Int -> Range
noBeacon sensor beacon yrow =
  let d = mDist sensor beacon
      dy = abs (yrow - snd sensor)
  in range (fst sensor) (d-dy)

noBRange :: Grid -> Int -> Range
noBRange grid rowY =
  let sensors = M.keys grid
      beacons = map fst $ filter (\b -> snd b == rowY) (M.elems grid)
  in rsUnion (map (\s -> noBeacon s (grid M.! s) rowY) sensors)
       `noPoints` beacons

part1 :: Grid -> Int
part1 grid = rLength (noBRange grid 2000000)

-- Part 2

findBR :: (Int,Int) -> Range -> Maybe Int
findBR (l,h) [] = Just l
findBR (l,h) ((l0,h0):r)
  | l<l0 = Just l
  | h<=h0 = Nothing
  | otherwise = findBR (h0+1,h) r

-- Searching from row y: search should finish if there is a solution
findB :: (Int,Int) -> Grid -> (Int,Int)
findB (x0,y0) grid = findBAux x0
  where findBAux y =
          case findBR (x0,y0) (noBRange grid y) of
            Just x -> (x,y)
            Nothing -> findBAux (y+1)

part2 :: Grid -> Int
part2 grid = let (x,y) = findB (0,20) grid
             in x*1000000 + y

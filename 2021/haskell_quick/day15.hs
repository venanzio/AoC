-- Advent of Code 2021, day 15

-- IDEA: part 2 still runs very slow, it needs a more efficient implementation of Dijkstra


module Main where

import System.Environment
import Data.List
import Data.Char

import Control.Applicative
import qualified Data.Map as M

import FunParser
import AoCTools
-- import Graphs

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let cave = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 cave))
  putStrLn ("Part 2: " ++ show (part2 cave))

-- Parsing the input

type Cave = [[Int]]

pInput :: Parser Cave
pInput = pLines digits

-- Part 1

type Pos = (Int,Int)

risk :: Cave -> Pos -> Int
risk cave (x,y) = cave!!x!!y

-- Indexing of all positions
allPos :: Int -> [Pos]
allPos size = [(i,j) | i <- [0..size-1], j <- [0..size-1]]

posIndex :: Int -> Pos -> Int
posIndex size (i,j) = i*size + j

neighbours :: Int -> Pos -> [Pos]
neighbours size (x,y) =
      [(x',y) | x' <- [x-1,x+1], x'>=0 && x'<size] ++
      [(x,y') | y' <- [y-1,y+1], y'>=0 && y'<size]

type Paths = M.Map Pos Int

dist :: Paths -> Pos -> Int
dist paths p = case M.lookup p paths of
  Just d -> d
  Nothing -> 1000000

pEmpty :: Paths
pEmpty = M.insert (0,0) 0 M.empty

merge :: Paths -> [Pos] -> [Pos] -> [Pos]
merge paths [] ys = ys
merge paths xs [] = xs
merge paths xs@(x:xs') ys@(y:ys') =
  if dist paths x <= dist paths y
  then x : merge paths xs' ys
  else y : merge paths xs ys'

dijkstra :: Int -> Cave -> Paths -> [Pos] -> [Pos] -> Paths
dijkstra size cave paths visited [] = paths
dijkstra size cave paths visited (p:seen) =
  let d = dist paths p
      ns = (neighbours size p \\ (visited ++ seen))
      paths' = foldl (\pts q -> M.insert q (d + risk cave q) pts) paths ns
  in dijkstra size cave paths' (p:visited) (merge paths' seen (sortOn (dist paths') ns))

shortest :: Cave -> Pos -> Pos -> Int
shortest cave p0 p1 =
  let size = length cave
  in dist (dijkstra size cave pEmpty [] [p0]) p1

part1 :: Cave -> Int
part1 cave = let size = length cave in shortest cave (0,0) (size-1,size-1)


-- Part 2

incr :: Int -> Int
incr x = if x<9 then x+1 else 1

incrCave :: Cave -> Cave
incrCave  = map (map incr)

caveJoin :: Cave -> Cave -> Cave
caveJoin = zipWith (++)

expandLeft :: Cave -> Cave
expandLeft cave = foldl1 caveJoin (take 5 $ iterate incrCave cave)

expand :: Cave -> Cave
expand cave = let row = expandLeft cave
              in concat $ take 5 $ iterate incrCave row

mapPathLeft :: Cave
mapPathLeft = undefined

part2 :: Cave -> Int
part2 cave = let bigCave = expand cave
                 size = length bigCave
             in shortest bigCave (0,0) (size-1,size-1)


-- displaying the cave

showCave :: Cave -> String
showCave = concat . map (\row -> concat (map show row) ++ "\n")

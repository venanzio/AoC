-- Advent of Code 2021, day 15

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
  putStrLn (show (length xs) ++ " , " ++ show (length (head xs)))
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type Cave = [[Int]]

pInput :: Parser Cave
pInput = pLines digits

-- Part 1

type Pos = (Int,Int)
type Paths = M.Map Pos Int

risk :: Cave -> Pos -> Int
risk cave (x,y) = cave!!x!!y

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


neighbours :: Cave -> Pos -> [Pos]
neighbours cave (x,y) =
  let size = length cave in
      [(x',y) | x' <- [x-1,x+1], x'>=0 && x'<size] ++
      [(x,y') | y' <- [y-1,y+1], y'>=0 && y'<size]

dijkstra :: Cave -> Paths -> [Pos] -> [Pos] -> Paths
dijkstra cave paths visited [] = paths
dijkstra cave paths visited (p:seen) =
  let size = length cave
      d = dist paths p -- (_,p,d) = minimumF (\p -> dist paths p) seen
      ns = (neighbours cave p \\ (visited ++ seen))
      paths' = foldl (\pts q -> M.insert q (d + risk cave q) pts) paths ns
  in dijkstra cave paths' (p:visited)   (merge paths' seen (sortOn (dist paths') ns)) -- ((seen ++ ns) \\ [p])

shortest :: Cave -> Int
shortest cave =
  let size = length cave
  in dist (dijkstra cave pEmpty [] [(0,0)]) (size-1,size-1)

part1 :: Cave -> Int
part1 = shortest

-- Part 2

incr :: Int -> Int
incr x = if x<9 then x+1 else 0

incrCave :: Cave -> Cave
incrCave  = map (map incr)

caveJoin :: Cave -> Cave -> Cave
caveJoin = zipWith (++)

expandLeft :: Cave -> Cave
expandLeft cave = foldl1 caveJoin (take 5 $ iterate incrCave cave)

expand :: Cave -> Cave
expand cave = let row = expandLeft cave
              in concat $ take 5 $ iterate incrCave row

part2 :: Cave -> Int
part2 cave = shortest (expand cave)

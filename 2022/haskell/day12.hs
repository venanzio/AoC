-- Advent of Code 2022, day 12
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

type Location = (Int,Int)
type Grid = M.Map Location Int

pLoc :: Parser [Char]
pLoc = many item

pLocs :: Parser (M.Map Location Char)
pLocs = pLines pLoc >>= return . mMap

sourceTarget :: M.Map Location Char -> (Location,Location)
sourceTarget m = let ass = M.toList m
                     [(lS,_)] = filter (\(_,c) -> c=='S') ass
                     [(lE,_)] = filter (\(_,c) -> c=='E') ass
                 in (lS,lE)

stElim :: M.Map Location Char -> M.Map Location Char
stElim = M.map (\c -> case c of
                   'S' -> 'a'
                   'E' -> 'z'
                   c -> c)

toGrid :: M.Map Location Char -> Grid
toGrid = M.map (\c -> ord c - ord 'a')

pInput :: Parser (Grid,Location,Location)
pInput = do m <- pLocs
            let (lS,lE) = sourceTarget m
            return (toGrid (stElim m), lS, lE)

-- Part 1

-- Use Dijkstra's algorithm, computing distances backwards from E
--   so we have all minimum distances from each location in one pass

oneStep :: Grid -> Location -> [Location]
oneStep grid (x1,y1) =
  filter (\l -> case M.lookup l grid of
             Nothing -> False
             Just h -> h <= grid M.! (x1,y1) + 1)
         [(x1,y1-1), (x1,y1+1), (x1-1,y1), (x1+1,y1)]

close :: Grid -> Location -> Location -> Bool
close grid (x2,y2) (x1,y1) = (x2,y2) `elem` oneStep grid (x1,y1)
  
relax :: Grid -> (Location,Int) -> (Location,Int) -> (Location,Int)
relax grid (l0,d0) (l1,d1) =
  (l1, if close grid l0 l1 then min d1 (d0 + 1) else d1)

dijkstra :: Grid -> [(Location,Int)] -> [Location] -> [(Location,Int)]
dijkstra _ [] _ = []
dijkstra grid visited locs =
  let ((l0,d0):visited1) = sortBy (\(_,d1) (_,d2) -> compare d1 d2) visited
      (newVis,locs1) = partition (close grid l0) locs
      visited2 = map (relax grid (l0,d0)) visited1 ++
                   (map (\l->(l, d0+1)) newVis)
  in (l0,d0) : dijkstra grid visited2 locs1

shortest :: (Grid,Location,Location) -> Int
shortest (grid,lS,lE) =
  let locDist = dijkstra grid [(lE,0)] (M.keys grid \\ [lE])
  in case find (\(l,_) -> l==lS) locDist of
       Just (_,d) -> d
       Nothing -> 1000
     
part1 :: (Grid,Location,Location) -> Int
part1 = shortest

-- Part 2

bestStart :: Grid -> Location -> Int
bestStart grid lE =
  head $ sort $
  map snd $
  filter (\(l,_) -> grid M.! l == 0) $
  dijkstra grid [(lE,0)] (M.keys grid \\ [lE])
 
part2 :: (Grid,Location,Location) -> Int
part2 (grid,_,lE) = bestStart grid lE

-- Advent of Code: Dijkstra's Algorithm

module Dijkstra where

import Data.List
import qualified Data.Map as M

import AoCTools

-- Graphs as adjacency lists (with integer weights)
type Graph a = M.Map a [(a,Int)]

type GraphF a = a -> [(a,Int)]

graph :: Ord a => GraphF a -> [a] -> Graph a
graph gf xs = graphAux xs M.empty
  where graphAux [] g = g
        graphAux (x:xs) g = let gfx = gf x
                                g' = M.insert x gfx g
                                ys = map fst gfx
                            in graphAux ((ys \\ M.keys g') ++ xs) g'

delIndex :: [a] -> Int -> [a]
delIndex xs i = let (front,back) = splitAt i xs
                in front ++ tail back

extractMin :: Ord a => [a] -> M.Map a Int -> (a,[a])
extractMin xs w = let (i,x,_) = minimumF (\x -> w M.! x) xs
                  in (x, delIndex xs i)
                      
reviseL :: Ord a => (a,Int) -> [(a,Int)] -> M.Map a Int -> M.Map a Int
reviseL (x,l) yws lengths =
  foldl (\ls (y,w) -> if l+w < (ls M.! y) then M.insert y (l+w) ls else ls)
        lengths yws

wDiff :: Eq a => [(a,Int)] -> [a] -> [(a,Int)]
wDiff [] _ = []
wDiff ((x,w):wxs) visited = if (x `elem` visited) then wDiff wxs visited
                            else (x,w) : wDiff wxs visited

dijkstraW :: Ord a => Graph a -> [a] -> [a] -> M.Map a Int -> M.Map a Int
dijcstraW g visited [] lengths = lengths
dijkstraW g visited seen lengths =
  let (x,seen') = extractMin seen lengths
      links = g M.! x `wDiff` visited
      lengths' = reviseL (x,lengths M.! x) links lengths
  in dijkstraW g (x:visited) (nub $ map fst links ++ seen') lengths'

dijkstra :: Ord a => GraphF a -> a -> M.Map a Int
dijkstra gf x = dijkstraW (graph gf [x]) [] [x] M.empty

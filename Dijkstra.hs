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

updateMin :: Ord a => a -> Int -> M.Map a Int -> M.Map a Int
updateMin x l m = case M.lookup x m of
  Nothing -> M.insert x l m
  Just l0 -> if l < l0 then M.insert x l m else m
                     
reviseL :: Ord a => (a,Int) -> [(a,Int)] -> M.Map a Int -> M.Map a Int
reviseL (x,l) yws lengths =
  foldl (\ls (y,w) -> updateMin y (l+w) ls)
        lengths yws



wDiff :: Eq a => [(a,Int)] -> [a] -> [(a,Int)]
wDiff [] _ = []
wDiff ((x,w):wxs) visited = if (x `elem` visited) then wDiff wxs visited
                            else (x,w) : wDiff wxs visited

dijkstraW :: Ord a => GraphF a -> a -> [a] -> [a] -> M.Map a Int -> Int
dijcstraW g y visited [] lengths = error "unreacheable goal"
dijkstraW g y visited seen lengths =
  let (x,seen') = extractMin seen lengths
      wx = case M.lookup x lengths of -- lengths M.! x
             Just w -> w
             Nothing -> error ("length not yet computed")
      links = g x `wDiff` visited
      seenNew = map fst links \\ seen
      lengths' = reviseL (x,wx) links lengths
  in if x == y then wx
     else dijkstraW g y (x:visited) (seenNew ++ seen') lengths'

dijkstra :: Ord a => GraphF a -> a -> a -> Int
dijkstra gf x y = dijkstraW gf y [] [x] (M.singleton x 0)

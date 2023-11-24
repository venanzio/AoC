-- Advent of Code 2022, day 16
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
  let cave = parseAll pInput input
  -- putStrLn ("Part 1: " ++ show (part1 cave))
  putStrLn ("Part 2: " ++ show (part2 cave))

-- Parsing the input

data Valve = Valve {
  vOpen :: Bool,
  vFlowRate :: Int,
  vTunnels :: [String]
  }
  deriving Show

type Cave = M.Map String Valve

pValves :: Parser [String]
pValves = do symbol "; tunnels lead to valves"
             someSepStr word ","
          <|>
          do symbol "; tunnel leads to valve"
             v <- word
             return [v]

pData :: Parser (String,Valve)
pData = do symbol "Valve"
           vName <- word
           symbol "has flow rate="
           fRate <- integer
           vs <- pValves
           return (vName, Valve False fRate vs)

pInput :: Parser Cave
pInput = do vs <- pLines pData
            return (M.fromList vs)

-- Part 1

{- Visit the valves with non-zero flow in all possible sequences
   use dijkstra to determine the shortest path to them.
-}

infinite = 30*100

dijkstra :: Cave -> String -> String -> Int
dijkstra cave s t = dijkstra_aux ((s,0):[(v,infinite) | v <- M.keys cave, v /= s])
  where dijkstra_aux queue =
          let (x,d):queue' = sortOn snd queue
              Just v = M.lookup x cave
          in if x==t then d
               else dijkstra_aux [(y, relax d v y dy) | (y,dy) <- queue']
        relax d v y dy = if y `elem` vTunnels v then min (d+1) dy else dy

-- pressure released by visiting valves in a given order

goodValves :: Cave -> [String]
goodValves cave = [x | (x,v) <- M.toList cave, vFlowRate v /= 0]

-- Pressure released on a given path thwrough "good" valves

pressure :: Cave -> [String] -> Int
pressure cave gvs = pressureFrom cave 30 "AA" gvs

pressureFrom :: Cave -> Int -> String -> [String] -> Int
pressureFrom cave min x [] = 0
pressureFrom cave min x (y:gvs) =
  let Just yv = M.lookup y cave
      flow = vFlowRate yv
      dist = dijkstra cave x y
      min0 = min - dist - 1
  in if min0 <= 0 then 0 else min0 * flow + pressureFrom cave min0 y gvs



-- best route among a list of valves

bestRoute :: Cave -> [String] -> Int
bestRoute cave gvs = fst $ bestRouteFrom cave 30 "AA" gvs

bestRouteFrom :: Cave -> Int -> String -> [String] -> (Int,[String])
bestRouteFrom cave _ _ [] = (0,[])
bestRouteFrom cave steps x gvs =
      maximum [let steps' = steps - dijkstra cave x y - 1
                   Just yv = M.lookup y cave
                   flow = steps' * vFlowRate yv
                   gvs' = delete y gvs
                   (flow_gvs', gvs'') = bestRouteFrom cave steps' y gvs'
               in if steps' <= 0 then (0,gvs) else (flow + flow_gvs',gvs'')
              | y <- gvs]

-- Always choose the valve that gives the highest pressure releas
-- Gives incorrect answer

greedyRoute :: Cave -> [String] -> Int
greedyRoute cave gvs = gRoute cave 30 "AA" gvs 

gRoute :: Cave -> Int -> String -> [String] -> Int
gRoute cave min vs gvs =
  let (_,vt,flow) = maximumF (vFlow cave min vs) gvs
      min' = min - dijkstra cave vs vt - 1   -- this is computed twice
      gvs' = delete vt gvs
  in if min' <= 0 then 0 else flow + gRoute cave min' vt gvs'

-- swapping elements to improve flow

swap :: [a] -> Int -> Int -> [a]
swap l i j =
  let (li,xi:lir) = splitAt i l
      (lij,xj:lj) = splitAt (j-i-1) lir
  in li ++ xj:lij ++ xi:lj

allSwaps :: [a] -> [[a]]
allSwaps l = [swap l i j | (i,j) <- indexPairs (length l)]

indexPairs :: Int -> [(Int,Int)]
indexPairs n = [(i,j) | i <- [0..n-1], j <- [0..n-1], i < j]

-- try to improve a sequence by swapping elements
swapRoute :: Cave -> [String] -> (Int,[String])
swapRoute cave gvs =
    let (_,vs,flow) = maximumF (pressure cave)  (allSwaps gvs)
    in (flow,vs)
    
swRoute :: Cave -> [String] -> Int
swRoute cave gvs =
  let flow0 = pressure cave gvs
      (flow1,vs) = swapRoute cave gvs
  in if flow0 >= flow1 then flow0
                       else swRoute cave vs
      



-- We move from vs to vt and open the valve

vFlow :: Cave -> Int -> String -> String -> Int
vFlow cave min vs vt =
  let steps = dijkstra cave vs vt
      fmin = min - steps -1  -- minutes the valve will be open
      Just valve = M.lookup vt cave
  in fmin * (vFlowRate valve)
          
part1 :: Cave -> Int
part1 cave = swRoute cave (goodValves cave)

-- Part 2

bestElephant :: Cave -> [String] -> Int
bestElephant cave gvs = bestElephant_aux 9 "AA" 9 "AA" gvs
  where bestElephant_aux _ _ _ _ [] = 0
        bestElephant_aux me mex el elx gvs =
            maximum [let me' = me - dijkstra cave mex y - 1
                         el' = el - dijkstra cave elx y - 1
                         Just yv = M.lookup y cave
                         gvs' = delete y gvs
                     in if me' <= 0 && el' <= 0 then 0
                          else if me' >= el'
                            then (me' * vFlowRate yv) + bestElephant_aux me' y el elx gvs'
                            else (el' * vFlowRate yv) + bestElephant_aux me mex el' y gvs'
                    | y <- gvs]
          
part2 :: Cave -> Int
part2 cave = fst $ bestRouteFrom cave 26 "AA" (goodValves cave) -- bestElephant cave (goodValves cave)

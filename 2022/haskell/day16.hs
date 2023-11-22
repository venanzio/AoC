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
  let xs = parseAll pInput input
  putStrLn (show (initQueue xs))
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

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

{- see the cave as a graph with nodes
   (v,b): v = valve, b = open or closed
   "distance" is the pressure release
   use Dijkstra to get maxmum "distance"
-}

type VQueue = [(String,Bool,Int)]

mostPressure :: VQueue -> (String,Bool,Int,VQueue)
mostPressure queue =
  let (v,b,p):queue' = sortOn (\(_,_,p)->p) queue
  -- relax the remaining elements
  in (v,b,p,queue')

initQueue :: Cave -> VQueue
initQueue cave = concat [[(v,False,0),(v,True,0)] | v <- M.keys cave]

{- Almost all flow rate are 0, we should optimize for that:
   Find the shortest path between any two valves
   then only consider higher-level paths between the valves
   that have non-zero flow rate, using the previous results
   to move between them (subtract the length of the path from the time)
-}

pressure :: Int -> String -> Cave -> (Int,[String])
pressure time vName cave = if time <= 1 then (0,[vName]) else
  let valve = cave M.! vName
      fr = if vOpen valve then 0 else vFlowRate valve
      (time',cave') =
        if fr == 0 then (time-1, cave)
                   else (time-2, M.insert vName (valve {vOpen = True}) cave)
      vns = vTunnels valve
      (_,(_,path),maxP) = maximumF fst ((0,[]):[pressure time' vn cave'| vn <- vns])
  in (fr * (time-1) + maxP, vName:path)
--      maximum (0:[pressure time' vn cave'| vn <- vns])
              
part1 :: Cave -> (Int,[String])
part1 cave = pressure 14 "AA" cave

-- Part 2

part2 :: Cave -> Int
part2 _ = 2

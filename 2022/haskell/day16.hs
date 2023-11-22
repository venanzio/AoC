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

-- best route among a list of valves

bestRoute :: Cave -> [String] -> Int
bestRoute cave gvs = bestRoute_aux 30 "AA" gvs
  where bestRoute_aux _ _ [] = 0
        bestRoute_aux steps x gvs =
          maximum [let steps' = steps - dijkstra cave x y - 1
                       Just yv = M.lookup y cave
                       flow = steps' * vFlowRate yv
                       gvs' = delete y gvs
                   in if steps' <= 0 then 0 else flow + bestRoute_aux steps' y gvs'
                  | y <- gvs]
                       
part1 :: Cave -> Int
part1 cave = bestRoute cave (goodValves cave)

-- Part 2

bestElephant :: Cave -> [String] -> Int
bestElephant cave gvs = bestElephant_aux 26 "AA" 26 "AA" gvs
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
part2 cave = bestElephant cave (goodValves cave)

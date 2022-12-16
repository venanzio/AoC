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
  putStrLn (show $ xs M.! "AA")
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

pressure :: Int -> String -> Cave -> Int
pressure time vName cave = if time <= 0 then 0 else
  let valve = cave M.! vName
      fr = if vOpen valve then 0 else vFlowRate valve
      time' = if fr == 0 then time-1 else time-2
      vns = vTunnels valve
      cave' = M.insert vName (valve {vOpen = True}) cave
  in fr * (time-1) +
      maximum (0:[pressure time' vn cave'
                 | vn <- vns])
              
part1 :: Cave -> Int
part1 cave = pressure 30 "AA" cave

-- Part 2

part2 :: Cave -> Int
part2 _ = 2

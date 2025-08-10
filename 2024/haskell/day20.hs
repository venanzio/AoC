-- Advent of Code 2024, day 20
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
-- import Data.Char
-- import Control.Applicative
import qualified Data.Map.Strict as M

import FunParser
import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let mp = stringsMap $ lines input
      wall = M.keys $ M.filter (=='#') mp
      gr = mazeGraph wall
      start = head $ mFind 'S' mp
      end = head $ mFind 'E' mp
      
  putStrLn ("Part 1: " ++ show (part1 mp))
  putStrLn ("Part 2: " ++ show (part2 mp))

-- Parsing the input

pData :: Parser ()
pData = return ()

pInput :: Parser [()]
pInput = pLines pData

-- Part 1

{-
-- Extended graph: each point has an int indicating the cheats
--  0 = no cheat, 1 = first step of the cheat, 2 = used the cheat
graph :: Map2D Char -> Graph (Point,Int)
graph mp = M.fromList [((p,i), map (\q->(q,1)) (step (p,i))) |
                       p <- allPoints pMin pMax, i <- [0,1,2]]
  where wall = M.keys (M.filter (=='#') mp)
        (pMin,pMax) = minMaxPoints wall
        free p = not $ p `elem` wall
        step (p,i) = case i of
          0 -> map (\q->(q,0)) fs ++ map (\q->(q,1)) ws
          1 -> map (\q->(q,2)) (neighboursHV p) 
          2 -> map (\q->(q,2)) fs
          where (fs,ws) = partition free (neighboursHV p) 
-}

mdGraph :: Map2D Char -> Graph Point
mdGraph md = M.fromList [(p,step p) | p <- allPoints pMin pMax]
  where (pMin,pMax) = minMaxPoints (M.keys md)
        step p = undefined

part1 :: Map2D Char -> Int
part1 md = undefined

-- Part 2

part2 :: Map2D Char -> Int
part2 _ = 2

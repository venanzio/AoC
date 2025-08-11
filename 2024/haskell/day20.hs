-- Advent of Code 2024, day 20
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
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
  let mp = stringsMap $ lines input
      (pMin,pMax) = minMaxPoints (M.keys mp)
      wall = M.keys $ M.filter (=='#') mp
      gr = mazeGraph wall
      free = M.keys gr
      start = head $ mFind 'S' mp
      end = head $ mFind 'E' mp
      fromS = dijkstraAll gr start
      toE = dijkstraAll gr end
      noCheat = fromS M.! end -- toE M.! start
      chs1 = cheats1 pMin pMax free
      chs2 = cheats2 pMin pMax free
  putStrLn ("shortest path (from S): " ++ show (fromS M.! end))
  putStrLn ("shortest path (to E): " ++ show (toE M.! start))
  putStrLn ("Part 1: " ++ show (bestCheats fromS toE chs1 (noCheat - 100)))
  putStrLn ("Part 2: " ++ show (bestCheats fromS toE chs2 (noCheat - 100)))

-- Parsing the input

pData :: Parser ()
pData = return ()

pInput :: Parser [()]
pInput = pLines pData

-- Puzzle

cheatTime :: M.Map Point Int -> M.Map Point Int -> Point -> Point -> Int
cheatTime fromS toE ch1 ch2 =
  fromS M.! ch1 + toE M.! ch2 + distM ch1 ch2

bestCheats :: M.Map Point Int -> M.Map Point Int -> [(Point,Point)] -> Int -> Int
bestCheats fromS toE chs maxd = length $
  filter (\(ch1,ch2) -> cheatTime fromS toE ch1 ch2 <= maxd) chs 

cheats1 :: Point -> Point -> [Point] -> [(Point,Point)]
cheats1 pMin pMax free = [(p,q) | p <- free, q <- free, distM p q == 2]

cheats2 :: Point -> Point -> [Point] -> [(Point,Point)]
cheats2 pMin pMax free = [(p,q) | p <- free, q <- free, distM p q <= 20]

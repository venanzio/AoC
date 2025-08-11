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
      (pMin,pMax) = minMaxPoints (M.keys mp)
      wall = M.keys $ M.filter (=='#') mp
      free = allPoints pMin pMax \\ wall
      gr =  mpGraph mp
      start = head $ mFind 'S' mp
      end = head $ mFind 'E' mp
      fromS = dijkstraAll gr start
      toE = dijkstraAll gr end
      chs = cheats pMin pMax wall
      noCheat = fromS M.! end -- toE M.! start
  putStrLn ("shortest path (from S): " ++ show (fromS M.! end))
  putStrLn ("shortest path (to E): " ++ show (toE M.! start))
  putStrLn ("Part 1: " ++ show (bestCheats fromS toE chs (noCheat - 100)))
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

mpGraph :: Map2D Char -> Graph Point
mpGraph mp = M.fromList [(p,step p) | p <- allPoints pMin pMax]
  where (pMin,pMax) = minMaxPoints (M.keys mp)
        step p = case M.lookup p mp of
          Just '#' -> []
          _        -> [(q,1) | q <- neighboursHV p, pInside pMin pMax q]

cheatTime :: M.Map Point Int -> M.Map Point Int -> Point -> Point -> Int
cheatTime fromS toE ch1 ch2 =
  fromS M.! ch1 + toE M.! ch2 + 1

cheats :: Point -> Point -> [Point] -> [(Point,Point)]
cheats pMin pMax wall = [(p,q) | p <- wall, q <- neighboursHV p,
                                 pInside pMin pMax q, not (q `elem` wall)]

bestCheats :: M.Map Point Int -> M.Map Point Int -> [(Point,Point)] -> Int -> Int
bestCheats fromS toE chs maxd = length $
  filter (\(ch1,ch2) -> cheatTime fromS toE ch1 ch2 <= maxd) chs 

part1 :: Map2D Char -> Int
part1 mp = 1
        

-- Part 2

cheats2 :: Point -> Point -> [Point] -> [(Point,Point)]
cheats2 pMin pMax free = [(p,q) | p <- free, q <- free, distM p q <= 20]

part2 :: Map2D Char -> Int
part2 _ = 2

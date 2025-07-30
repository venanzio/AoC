-- Advent of Code 2024, day 12
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
-- import Data.Char
-- import Control.Applicative
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
  let m = stringsMap (lines input)
  putStrLn (show (boundary $ fst $ region m))
  putStrLn ("Part 1: " ++ show (part1 m))
  putStrLn ("Part 2: " ++ show (part2 m))

-- Part 1

-- Separate a region from the rest of the map
region :: Map2D Char -> ([Point], Map2D Char)
region m
  | M.null m = ([],m)
  | otherwise  = let (p,kind) = M.findMin m in
      regionAux [p] kind m

regionAux :: [Point] -> Char -> Map2D Char -> ([Point], Map2D Char)
regionAux [] kind m = ([],m)
regionAux (p:ps) kind m
  | M.lookup p m == Just kind =
       mapFst (p :) $ regionAux (neighboursHV p ++ ps) kind (M.delete p m)
  | otherwise = regionAux ps kind m

-- Perimeter of a region
perimeter :: [Point] -> Int
perimeter [] = 0
perimeter (p:ps) = 4 - 2 * length [q | q <- neighboursHV p, q `elem` ps]
                     + perimeter ps

price :: [Point] -> Int
price r = length r * perimeter r

tPrice :: Map2D Char -> Int
tPrice m
  | M.null m = 0
  | otherwise = let (r0,m0) = region m in price r0 + tPrice m0

part1 :: Map2D Char -> Int
part1 = tPrice

-- Part 2

boundary :: [Point] -> [(Point,Direction)]
boundary ps = nub [(c,q) | p <- ps, c <- corners p, q <- bDirections c ps]

corners p = [p, pMove p dRight, pMove p dDown, pMove p (1,1)]

bDirections :: Point -> [Point] -> [Direction]
bDirections p ps = [q | q <- directionsHV, isBoundary p q]
  where isBoundary p q = (length $ filter (`elem` ps) [edgeR p q, edgeL p q]) == 1

edgeR :: Point -> Direction -> Point
edgeR p d
  | d == dRight = p
  | d == dUp    = pMove p dUp
  | d == dLeft  = pMove p (-1,-1)
  | d == dDown  = pMove p dLeft

edgeL :: Point -> Direction -> Point
edgeL p d
  | d == dRight = pMove p dUp
  | d == dUp    = pMove p (-1,-1)
  | d == dLeft  = pMove p dLeft
  | d == dDown  = p
 
isBoundary q ps = any (\p -> not (p `elem` ps))
                      [q, pMove q dUp, pMove q dLeft, pMove q (-1,-1)]

tour :: (Point,Direction) -> (Point,Direction) -> [(Point,Direction)]
        -> (Int, [(Point,Direction)])
tour (p0,d0) (p,d) pds
  | p == p0 = (if d == d0 then 0 else 1, pds0)
  | otherwise = undefined
  where p1 = pMove p d
        pds0 = delete (p,d) $ delete (p1,pNeg d) pds 
                  
part2 :: Map2D Char -> Int
part2 _ = 2

-- Advent of Code 2021, day 17

-- IDEA: maybe there is a more elegant solution using linear programming
-- TO IMPROVE: parsing of the input instead of writing it in constants

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
  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)

-- Parsing the input

-- Target area
x0 = 195
x1 = 238
y0 = -93
y1 = -67

-- Part 1

data Check = Under | Inside | Over
  deriving (Eq,Show)

type Position = (Int,Int)
type Velocity = (Int,Int)

trajectory :: Position -> Velocity -> [Position]
trajectory (x,y) (vx,vy) =
  let x' = x+vx
      y' = y+vy
  in (x',y'): trajectory (x',y') (vx - (signum vx), vy - 1)

hit :: [Position] -> Maybe Position
hit ((x,y):ps) | (x>=x0 && x<=x1) &&  (y>=y0 && y<=y1) = Just (x,y)
               | y<y0      = Nothing
               | otherwise = hit ps

maxY :: [Position] -> Int
maxY ((_,y1):ps@((_,y2):_)) =
  if y1>y2 then y1 else maxY ps

maxH :: Velocity -> Int
maxH v = maxY (trajectory (0,0) v)

checkX :: Int -> Check
checkX x = if x<x0 then Under
            else if x>x1 then Over else Inside

checkY :: Int -> Check
checkY y = if y<y0 then Under
            else if y>y1 then Over else Inside

stepX :: (Int,Int,Int) -> (Int,Int,Int)
stepX (vx0,x,vx) = (vx0,x + vx, vx - signum vx)

stepY :: (Int,Int,Int) -> (Int,Int,Int)
stepY (vy0,y,vy) = (vy0,y + vy, vy - 1)

pVel0 (v0,_,_) = v0
pVel (_,_,v) = v
pPos (_,p,_) = p

xFilter (_,x,vx) = case checkX x of
  Under -> vx>0
  Inside -> True
  Over -> False

yFilter (_,y,vy) = checkY y /= Under

-- xvps: list of (vx0,x,vx) initial velocity and present position and velocity
-- result: for every index i, list of initial x velocities with a hit at i
hitsX :: [(Int,Int,Int)] -> [[Int]]
hitsX [] = []
hitsX xvps = map pVel0 (filter (\xvp -> checkX (pPos xvp) == Inside) xvps) :
             hitsX (filter xFilter (map stepX xvps))


hitsY :: [(Int,Int,Int)] -> [[Int]]
hitsY [] = []
hitsY yvps = map pVel0 (filter (\yvp -> checkY (pPos yvp) == Inside) yvps) :
             hitsY (filter yFilter (map stepY yvps))

xInit :: [(Int,Int,Int)]
xInit = map (\vx0 -> (vx0,0,vx0)) [1..x1+1]

yInit :: [(Int,Int,Int)]
yInit = map (\vy0 -> (vy0,0,vy0)) [y0-1..2*x1+1]

combine (vxs,vys) = [(vx,vy) | vx <- vxs, vy <- vys]

hitV :: [Velocity]
hitV = nub $ concat $ (map combine) (zip (hitsX xInit) (hitsY yInit))

part1 :: Int
part1 = maximum (map maxH hitV)

-- Part 2

part2 :: Int
part2 = length hitV

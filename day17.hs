-- Advent of Code 2021, day 17

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
  -- input <- readFile fileName
  -- let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)

-- Parsing the input

pData :: Parser ()
pData = return ()

pInput :: Parser [()]
pInput = pLines pData

x0 = 195
x1 = 238
y0 = -93
y1 = -67

{- example
x0 = 20
x1 = 30
y0 = -10
y1 = -5
-}

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


xTraj :: Int -> Int -> [Int]
xTraj x vx = let x' = x+vx in x' : xTraj x' (vx - signum vx)

xHit :: [Int] -> [Int]
xHit xs = xHitIndex 1 xs
  where xHitIndex n (x:xs) | x>=x0 && x<=x1 = n : xHitIndex (n+1) xs
                           | x>x1 = []
                           | otherwise = xHitIndex (n+1) xs



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
hitV = concat $ (map combine) (zip (hitsX xInit) (hitsY yInit))






{-
xVels :: [Int]
xVels = filter (xHit . xTraj 0) [1..x1+1]


yTraj :: Int -> Int -> [Int]
yTraj y vy = let y' = y+vy in y' : yTraj y' (vy - 1)

yHit :: [Int] -> Bool
yHit (y:ys) = if y>=y0 then y<=y1 else yHit ys

yVels :: [Int]
yVels = filter (yHit . yTraj 0) [y0-1..x1+1]



maxHeight :: Int
maxHeight =
  foldl (\my v -> if (maxYHit v) > my then (maxYHit v) else my) 0 [(vx,vy) | vx <- xVels, vy <- yVels]
    where maxYHit v = let t = trajectory (0,0) v
                      in case hit t of
                           Just _ -> maxY t
                           Nothing -> 0
-}

part1 :: Int
part1 = maximum (map maxH hitV)

-- Part 2

part2 :: Int
part2 = 2

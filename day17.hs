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

{-
x0 = 195
x1 = 238
y0 = -93
y1 = -67
-}

x0 = 20
x1 = 30
y0 = -10
y1 = -5

-- Part 1

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
part1 = 1

-- Part 2

part2 :: Int
part2 = 2

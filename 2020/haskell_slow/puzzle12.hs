-- Advent of Code 2020, day 12

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Data types

type Action = (Char,Int)

-- first two numbers are coordinates, third is the direction
type Ship = (Int,Int,Int)

shipPos :: Ship -> (Int,Int)
shipPos (x,y,_) = (x,y)

-- Parsing the input

action :: Parser Char
action = char 'N' <|> char 'S'  <|> char 'E' <|> char 'W' <|>
         char 'L'  <|> char 'R'  <|> char 'F'

pData :: Parser Action
pData = do act <- action
           value <- natural
           return (act,value)

pInput :: Parser [Action]
pInput = pLines pData

-- Part 1

move :: Ship -> Action -> Ship
move (x,y,d) ('N',n) = (x,y+n,d)
move (x,y,d) ('S',n) = (x,y-n,d)
move (x,y,d) ('E',n) = (x+n,y,d)
move (x,y,d) ('W',n) = (x-n,y,d)
move (x,y,d) ('L',n) = (x,y, (d+n) `mod` 360)
move (x,y,d) ('R',n) = (x,y, (d-n) `mod` 360)
move (x,y,d) ('F',n) -- assume the direction is a multiple of 90
  | d==0                 = (x+n,y,d)
  | d==90  || d==(-270)  = (x,y+n,d)
  | d==180 || d ==(-180) = (x-n,y,d)
  | d==270 || d ==(-90)  = (x,y-n,d)
  | otherwise            = error ("wrong direction: " ++ show d)

navigate :: Ship -> [Action] -> Ship
navigate = foldl move

mDistance :: (Int,Int) -> Int
mDistance (x,y) = abs x + abs y

part1 :: [Action] -> Int
part1 acts = mDistance $ shipPos $ navigate (0,0,0) acts

-- Part 2

type ShipW = ((Int,Int),(Int,Int))  -- position of ship + waypoint

-- moving the waypoint
moveW :: (Int,Int) -> Action -> (Int,Int)
moveW (x,y) ('N',n) = (x,y+n)
moveW (x,y) ('S',n) = (x,y-n)
moveW (x,y) ('E',n) = (x+n,y)
moveW (x,y) ('W',n) = (x-n,y)
moveW (x,y) ('L',n)
  | n==0                 = (x,y)
  | n==90 || n==(-270)   = (-y,x)
  | n==180 || n ==(-180) = (-x,-y)
  | n==270 || n ==(-90)  = (y,-x)
  | otherwise            = error ("wrong direction" ++ show n)
moveW (x,y) ('R',n) = moveW (x,y) ('L',-n)
moveW (x,y) ('F',n) = error "Can't do an F on the waypoint"

-- moving the ship
moveSW  :: ShipW -> Action -> ShipW
moveSW ((x,y),w@(dx,dy)) ('F',n) = ((x+n*dx,y+n*dy),w)
moveSW (s,w) a = (s,moveW w a)

navigateW :: ShipW -> [Action] -> ShipW
navigateW = foldl moveSW

part2 :: [Action] -> Int
part2 acts = mDistance $ fst $ navigateW ((0,0),(10,1)) acts


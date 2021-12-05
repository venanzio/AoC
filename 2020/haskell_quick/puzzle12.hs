module PuzzleInput where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

puzzle :: String -> IO Int
puzzle fileName = do
  input <- readFile fileName
  let as = parseAll actions input
      -- (x,y,_) = navigate (0,0,0) as
      ((x,y),_) = navShip ((0,0),(10,1)) as
  return (abs $ x+y)

type Action = (Char,Int)

type Ship = (Int,Int,Int)  -- position + direction

-- Part 1

act :: Parser Char
act = token (char 'N' <|> char 'S'  <|> char 'E' <|> char 'W' <|>
             char 'L'  <|> char 'R'  <|> char 'F')

actions :: Parser [Action]
actions = some (act >>= \a -> natural >>= \n -> return (a,n))

move :: Ship -> Action -> Ship
move (x,y,d) ('N',n) = (x,y+n,d)
move (x,y,d) ('S',n) = (x,y-n,d)
move (x,y,d) ('E',n) = (x+n,y,d)
move (x,y,d) ('W',n) = (x-n,y,d)
move (x,y,d) ('L',n) = (x,y, (d+n) `mod` 360)
move (x,y,d) ('R',n) = (x,y, (d-n) `mod` 360)
move (x,y,d) ('F',n)
  | d==0                 = (x+n,y,d)
  | d==90 || d==(-270)   = (x,y+n,d)
  | d==180 || d ==(-180) = (x-n,y,d)
  | d==270 || d ==(-90)  = (x,y-n,d)
  | otherwise            = error ("wrong direction" ++ show d)

navigate :: Ship -> [Action] -> Ship
navigate = foldl move

-- Part 2
-- now Ship means WayPoint

type ShWay = ((Int,Int),(Int,Int))  -- position of ship + waypoint

mvShip :: ShWay -> Action -> ShWay
mvShip ((u,v),w@(x,y)) ('F',n) = ((u+n*x,v+n*y),w)
mvShip (p,w) a = (p,moveW w a)

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

navShip :: ShWay -> [Action] -> ShWay
navShip = foldl mvShip

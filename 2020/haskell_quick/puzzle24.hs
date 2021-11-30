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
  let tiles = map path (lines input)
      floor = finalFloor tiles
      floor' = daysFlip floor 100
  -- return (countBlack floor)
  return (countBlack floor')

data Direction = E | SE | SW | W | NW | NE
  deriving (Eq,Show)
type Coords = (Int,Int)
type Path = [Direction]

path :: String -> Path
path "" = []
path ('s':'e':s) = SE : path s
path ('s':'w':s) = SW : path s
path ('n':'e':s) = NE : path s
path ('n':'w':s) = NW : path s
path ('e':s) = E : path s
path ('w':s) = W : path s

-- Part 1

type Floor = M.Map Coords Bool -- True = White, False = Black

origin :: Coords
origin = (0,0)

move :: Coords -> Direction -> Coords
move (x,y) E  = (x+1,y  )
move (x,y) W  = (x-1 ,y  )
move (x,y) NE = (x  ,y+1)
move (x,y) SW = (x  ,y-1)
move (x,y) SE = (x+1,y-1)
move (x,y) NW = (x-1,y+1)


pathCoords :: Path -> Coords
pathCoords = foldl move origin

whiteFloor :: Floor
whiteFloor = M.empty

cTile :: Coords -> Floor -> Bool
cTile xy fl = case M.lookup xy fl of
  Nothing -> True
  Just b  -> b
  
tile :: Path -> Floor -> Bool
tile p = cTile (pathCoords p)

flipTile :: Path -> Floor -> Floor
flipTile p fl = let xy = pathCoords p
                in M.insert xy (not (tile p fl)) fl

finalFloor :: [Path] -> Floor
finalFloor = foldl (flip flipTile) whiteFloor

countBlack :: Floor -> Int
countBlack = M.foldr (\b n -> if b then n else n+1) 0

-- Part 2

type Flips = M.Map Coords Int

flips :: Coords -> Flips -> Int
flips xy flp = case M.lookup xy flp of
  Nothing -> 0
  Just n  -> n

addFlip :: Coords -> Flips -> Flips
addFlip xy flp = M.insert xy (flips xy flp + 1) flp

flipNeighbours :: Coords -> Flips -> Flips
flipNeighbours xy flp = foldl (flip addFlip) flp $ map (move xy) [E,SE,SW,W,NW,NE]
 
allFlips :: Floor -> Flips
allFlips = M.foldrWithKey (\xy b -> if b then id else flipNeighbours xy) M.empty

rule :: Bool -> Int -> Bool
rule False n = n==0 || n>2
rule True  n = n/=2

flipsFloor :: Floor -> Flips -> Floor
flipsFloor fl = M.mapWithKey (\xy n -> rule (cTile xy fl) n) 

dayFlip :: Floor -> Floor
dayFlip fl = flipsFloor fl (allFlips fl)

daysFlip :: Floor -> Int -> Floor
daysFlip fl 0 = fl
daysFlip fl n = daysFlip (dayFlip fl) (n-1)

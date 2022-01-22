-- Advent of Code 2020, day 11

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
  input <- readFile fileName
  let area = parseAll pArea input
  putStrLn ("Part 1: " ++ show (part1 area))
  putStrLn ("Part 2: " ++ show (part2 area))

-- Data Structures

data Seat = Floor | Occupied | Empty | NoSeat
  deriving (Eq,Show)

-- Seating area by coordinates
type Area = (Int, Int, M.Map (Int,Int) Seat)

aWidth :: Area -> Int
aWidth (w,_,_) = w

aLength :: Area -> Int
aLength (_,l,_) = l

area :: Area -> M.Map (Int,Int) Seat
area (_,_,a) = a

seat :: Area -> (Int,Int) -> Seat
seat (w,l,a) (i,j) = if 0<=i && i<w && 0<=j && j<l then a M.! (i,j) else NoSeat

-- Parsers for the input

pSeat :: Parser Seat
pSeat = (char '.' >> return Floor) <|>
        (char 'L' >> return Empty) <|>
        (char '#' >> return Occupied)
        
pSLine :: Parser [Seat]
pSLine = some pSeat

pSeats :: Parser [[Seat]]
pSeats = some (token pSLine)

mArea :: [[Seat]] -> Area
mArea xss = (length (head xss), length xss, mMap xss)

pArea :: Parser Area
pArea = pSeats >>= return.mArea

-- Part 1

directions :: [(Int,Int)]
directions = [(u,v) | u <- [-1,0,1], v <- [-1,0,1], (u,v) /= (0,0)]

type View = Area -> (Int,Int) -> (Int,Int) -> Seat

view1 :: View
view1 a (i,j) (u,v) = seat a (i+u,j+v)

view2 :: View
view2 a (i,j) (u,v) =
  let (i',j') = (i+u,j+v)
      s = seat a (i',j')
  in if s == Floor then view2 a (i',j') (u,v) else s

{-
neighbours :: (Int,Int) -> [(Int,Int)]
neighbours (i,j) = map (\(u,v)->(i+u,j+v)) directions

-- coordinates of neighbours
nCoords :: Int -> Int -> (Int,Int) -> [(Int,Int)]
nCoords w l (i,j)=
  [(i+u,j+v) | u<-[-1,0,1], v<-[-1,0,1],
               ((u,v) /= (0,0) &&
                i+u>=0 && i+u<w &&
                j+v>=0 && j+v<l)]
-}

-- count the occupied places in the neighbourhood
nCount :: View -> Area -> (Int,Int) -> Int
nCount view a (i,j) = length $ filter (==Occupied) $
   map (view a (i,j)) directions

{-
  
  map (seat a) $
  nCoords (aWidth a) (aLength a) (i,j)
-}

-- the type of rules to change the seat according to the view count
type Rule = Seat -> Int -> Seat

-- specific rule from the puzzle
-- the limit at witch an occupied seat becomes empty is a parameter
pRule :: Int -> Rule
pRule m Empty 0 = Occupied
pRule m Occupied n | n>=m = Empty
pRule _ s _ = s

-- next round
--  returns Nothing if the area hasn't changed
nextA :: View -> Rule -> Area -> Maybe Area
nextA view rule a =
  let ruleChanged ch x n = let x' = rule x n in (ch || x/=x',x')
      (changed, a') = M.mapAccumWithKey (\ch ij x -> ruleChanged ch x (nCount view a ij))
                                        False (area a)
  in if changed then Just (aWidth a, aLength a, a')
                else Nothing
                     
-- final state, after it stabilize
final :: View -> Rule -> Area -> Area
final view rule a = case nextA view rule a of
            Nothing -> a
            Just a' -> final view rule a'

countOccupied :: Area -> Int
countOccupied = M.size . M.filter (==Occupied) . area

part1 :: Area -> Int
part1 a = countOccupied (final view1 (pRule 4) a)

-- Part 2

{-
-- List of coordinates in one direction (going on forever)
dirList :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
dirList (i,j) (u,v) =
  let (i',j') = (i+u,j+v)
  in (i',j') : dirList (i',j') (u,v)

-- Check if coordinates are within the area with given width and length
valid :: Int -> Int -> (Int,Int) -> Bool;
valid width length (i,j) = i>=0 && i<width &&
                           j>=0 && j<length

-- first seat (empty or occupied) in a given direction
aView :: Area -> (Int,Int) -> (Int,Int) -> Seat
aView a p d = aListView a (dirList p d)

-- first seat from a list of coordinates
aListView :: Area -> [(Int,Int)] -> Seat
aListView a (p:ps)
  | valid (aWidth a) (aLength a) p = case seat a p of
      Occupied -> Occupied
      Empty -> Empty
      Floor -> aListView a ps
aListView a _ = Empty

-- count occupied seats visible from a position
vCount :: Area -> (Int,Int) -> Int
vCount a (i,j) =
  let dirs = [(u,v) | u<-[-1,0,1], v<-[-1,0,1], (u,v) /= (0,0)]
      ns = map (aView a (i,j)) dirs
  in length $ filter (==Occupied) ns

-- apply the rule with n the number of visible seats
ruleV :: Seat -> Int -> Seat
ruleV Empty 0 = Occupied
ruleV Occupied n | n>=5 = Empty
ruleV s _ = s


-- next round (same as nextA but using ruleV)
nextV :: Area -> Maybe Area
nextV a =
  let ruleChanged ch x n = let x' = ruleV x n in (ch || x/=x',x')
      (changed, a') = M.mapAccumWithKey (\ch ij x -> ruleChanged ch x (vCount a ij)) False (area a)
  in if changed then Just (aWidth a, aLength a, a')
                else Nothing
                     
-- final state, after it stabilize
finalV :: Area -> Area
finalV a = case nextV a of
             Nothing -> a
             Just a' -> finalV a'
-}

part2 :: Area -> Int
part2 a = countOccupied (final view2 (pRule 5) a)


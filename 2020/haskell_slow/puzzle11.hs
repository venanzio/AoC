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

data Seat = Floor | Occupied | Empty
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
seat (w,l,a) (i,j) = if 0<=i && i<w && 0<=j && j<l then a M.! (i,j)
  else error ("coordinates out of bound: width = " ++ show w ++ ", length = " ++ show l ++
              ", coordinates = " ++ show (i,j))


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

-- coordinates of neighbours
nCoords :: Int -> Int -> (Int,Int) -> [(Int,Int)]
nCoords w l (i,j)=
  [(i+u,j+v) | u<-[-1,0,1], v<-[-1,0,1],
               ((u,v) /= (0,0) &&
                i+u>=0 && i+u<w &&
                j+v>=0 && j+v<l)]
                
-- count the occupied places in the neighbourhood
nCount :: Area -> (Int,Int) -> Int
nCount a (i,j) = length $
  filter (==Occupied) $
  map (seat a) $
  nCoords (aWidth a) (aLength a) (i,j)

-- applied the rule to a seat with a given number of neighbours
rule :: Seat -> Int -> Seat
rule Empty 0 = Occupied
rule Occupied n | n>=4 = Empty
rule s _ = s

-- next round
--  returns Nothing if the area hasn't changed
nextA :: Area -> Maybe Area
nextA a =
  let ruleChanged ch x n = let x' = rule x n in (ch || x/=x',x')
      (changed, a') = M.mapAccumWithKey (\ch ij x -> ruleChanged ch x (nCount a ij)) False (area a)
  in if changed then Just (aWidth a, aLength a, a')
                else Nothing
                     
-- final state, after it stabilize
final :: Area -> Area
final a = case nextA a of
            Nothing -> a
            Just a' -> final a'

countOccupied :: Area -> Int
countOccupied = M.size . M.filter (==Occupied) . area

part1 :: Area -> Int
part1 a = countOccupied (final a)

-- Part 2

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

part2 :: Area -> Int
part2 a = countOccupied (finalV a)


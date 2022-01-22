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

-- type of functions that give the position visible in a direction
type View = Area -> (Int,Int) -> (Int,Int) -> Seat

-- View for part 1: next position in the direction
view1 :: View
view1 a (i,j) (u,v) = seat a (i+u,j+v)

-- count the occupied places in the neighbourhood
nCount :: View -> Area -> (Int,Int) -> Int
nCount view a (i,j) = length $ filter (==Occupied) $
   map (view a (i,j)) directions

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

-- View for part 2: first seat in the direction
view2 :: View
view2 a (i,j) (u,v) =
  let (i',j') = (i+u,j+v)
      s = seat a (i',j')
  in if s == Floor then view2 a (i',j') (u,v) else s

part2 :: Area -> Int
part2 a = countOccupied (final view2 (pRule 5) a)


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
  let area = parseAll pSeats input
  return (countOccupied $ final area)  -- Part 1
  -- return (countOccupied $ finalV area)  -- Part 2


data Seat = Floor | Occupied | Empty
  deriving (Eq,Show)

type Area = [[Seat]]

pSeat :: Parser Seat
pSeat = (char '.' >> return Floor) <|>
        (char 'L' >> return Empty) <|>
        (char '#' >> return Occupied)

pSLine :: Parser [Seat]
pSLine = some pSeat

pSeats :: Parser Area
pSeats = some (token pSLine)

-- Part 1

seat :: Area -> (Int,Int) -> Seat
seat a (i,j) = a!!i!!j

nCoords :: Int -> Int -> (Int,Int) -> [(Int,Int)]
nCoords rows cols (i,j)=
  [(i+u,j+v) | u<-[-1,0,1], v<-[-1,0,1],
               ((u,v) /= (0,0) &&
                i+u>=0 && i+u<rows &&
                j+v>=0 && j+v<cols)]
                

nCount :: Area -> (Int,Int) -> Int
nCount a (i,j) = length $
  filter (==Occupied) $
  map (seat a) $
  nCoords (length a) (length (head a)) (i,j)

rule :: Seat -> Int -> Seat
rule Empty 0 = Occupied
rule Occupied n | n>=4 = Empty
rule s _ = s

nextR :: Area -> Area
nextR a = [[rule (seat a (i,j)) (nCount a (i,j))
           | j <- [0..length (head a)-1]]
          | i <- [0..length a-1]]

final :: Area -> Area
final a = let a' = nextR a
          in if a'==a then a
                      else final a'

countOccupied :: Area -> Int
countOccupied = length . filter (==Occupied) . concat 

-- Part 2

-- List of coordinates in one direction
dirList :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
dirList (i,j) (u,v) =
  let (i',j') = (i+u,j+v)
  in (i',j') : dirList (i',j') (u,v)

valid :: Int -> Int -> (Int,Int) -> Bool;
valid rows cols (i,j) = i>=0 && i<rows &&
                        j>=0 && j<cols
  

aListView :: Area -> Int -> Int -> [(Int,Int)] -> Seat
aListView a rows cols (p:ps)
  | valid rows cols p = case seat a p of
      Occupied -> Occupied
      Empty -> Empty
      Floor -> aListView a rows cols ps
aListView a rows cols _ = Empty

aView :: Area -> Int -> Int -> (Int,Int) -> (Int,Int) -> Seat
aView a rows cols p d = aListView a rows cols (dirList p d)


vCount :: Area -> (Int,Int) -> Int
vCount a (i,j) =
  let rows = length a
      cols = length (head a)
      dirs = [(u,v) | u<-[-1,0,1], v<-[-1,0,1], (u,v) /= (0,0)]
      ns = map (aView a rows cols (i,j)) dirs
  in length $ filter (==Occupied) ns

ruleV :: Seat -> Int -> Seat
ruleV Empty 0 = Occupied
ruleV Occupied n | n>=5 = Empty
ruleV s _ = s


nextV :: Area -> Area
nextV a = [[ruleV (seat a (i,j)) (vCount a (i,j))
           | j <- [0..length (head a)-1]]
          | i <- [0..length a-1]]

finalV :: Area -> Area
finalV a = let a' = nextV a
           in if a'==a then a
                       else finalV a'


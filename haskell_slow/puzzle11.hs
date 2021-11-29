-- Advent of Code 2020, day 11

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
  {-
  input <- readFile fileName
  let area = parseAll pSeats input
      a1 = nextR area
  -}
  putStrLn ("Part 1: ") -- ++ show (part1 area))
  putStrLn ("Part 2: ") -- ++ show (countOccupied $ finalV area))

-- Data Structures

data Seat = Floor | Occupied | Empty
  deriving (Eq,Show)

-- Seating area by coordinates
type Area = (Int, Int, M.Map (Int,Int) Seat)

aWidth :: Area -> Int
aWidth (w,_,_) = w

aLength :: Area -> Int
aLength (_,l,_) = l

seat :: Area -> (Int,Int) -> Seat
seat (w,l,a) (i,j) = if i<w && l<l then a M.! (i,j) else error "coordinates out of bound"


-- Parsers for the input

pSeat :: Parser Seat
pSeat = (char '.' >> return Floor) <|>
        (char 'L' >> return Empty) <|>
        (char '#' >> return Occupied)
        
pSLine :: Parser [Seat]
pSLine = some pSeat

pSeats :: Parser [[Seat]]
pSeats = some (token pSLine)

-- Indexed maps: to be moved into a separate module

-- list to index map, with initial index
listMap :: Int -> [a] -> M.Map Int a
listMap i = M.fromAscList . (zip [i..])

-- 2-dimentional matrix to index map, with initial coordinates
matrixMap :: (Int,Int) -> [[a]] -> M.Map (Int,Int) a
matrixMap (row,col) xss = M.fromAscList [((r,c), xss!!r!!c) |
                                         r <- [0 .. length xss - 1],
                                         c <- [0 .. length (xss!!r) -1]]

mMap :: [[a]] -> M.Map (Int,Int) a
mMap = matrixMap (0,0)

pArea :: Parser Area
pArea = pSeats >>= \ss -> return (length (head ss), length ss, mMap ss)

-- Part 1

-- coordinates of neighbours
nCoords :: Int -> Int -> (Int,Int) -> [(Int,Int)]
nCoords rows cols (i,j)=
  [(i+u,j+v) | u<-[-1,0,1], v<-[-1,0,1],
               ((u,v) /= (0,0) &&
                i+u>=0 && i+u<rows &&
                j+v>=0 && j+v<cols)]
                
-- count the occupied places in the neighbourhood
nCount :: Area -> (Int,Int) -> Int
nCount a (i,j) = length $
  filter (==Occupied) $
  map (seat a) $
  nCoords (aLength a) (aWidth a) (i,j)


{-



-- applied the rule to a seat with a given number of neighbours
rule :: Seat -> Int -> Seat
rule Empty 0 = Occupied
rule Occupied n | n>=4 = Empty
rule s _ = s

-- next generation
-- Q: should be a maybe to remember if anything has been changed
nextR :: Area -> Area
nextR a = [[rule (seat a (i,j)) (nCount a (i,j))
           | j <- [0..aWidth a-1]]
          | i <- [0..aLength a-1]]

-- final state, after it stabilize
final :: Area -> Area
final a = let a' = nextR a
          in if a'==a then a
                      else final a'

countOccupied :: Area -> Int
countOccupied = length . filter (==Occupied) . concat 

part1 :: Area -> Int
part1 a = countOccupied (final a)

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
  let rows = aLength a
      cols = aWidth a
      dirs = [(u,v) | u<-[-1,0,1], v<-[-1,0,1], (u,v) /= (0,0)]
      ns = map (aView a rows cols (i,j)) dirs
  in length $ filter (==Occupied) ns

ruleV :: Seat -> Int -> Seat
ruleV Empty 0 = Occupied
ruleV Occupied n | n>=5 = Empty
ruleV s _ = s


nextV :: Area -> Area
nextV a = [[ruleV (seat a (i,j)) (vCount a (i,j))
           | j <- [0..aWidth a-1]]
          | i <- [0..aLength a-1]]

finalV :: Area -> Area
finalV a = let a' = nextV a
           in if a'==a then a
                       else finalV a'

-}

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


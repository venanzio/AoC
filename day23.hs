-- Advent of Code 2021, day 23

module Main where

import System.Environment
import Data.List
import Data.Char

import Control.Applicative
import qualified Data.Map as M

import FunParser
import AoCTools
import Dijkstra

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

-- Parsing the input
data Antiphod = A | B | C | D | E
  deriving (Eq,Show,Ord)
type Burrow = [[Antiphod]]

hall :: Burrow -> [Antiphod]
hall b = b!!0

room :: Burrow -> Int -> [Antiphod]
room b i = b!!i

pSkip :: Parser ()
pSkip = many (sat (\c -> c`elem` ".#" || isSpace c)) >> return ()

pAntiphod :: Parser Antiphod
pAntiphod = do pSkip
               (char 'A' >> return A) <|>
                 (char 'B' >> return B) <|>
                   (char 'C' >> return C) <|>
                     (char 'D' >> return D)

pRooms :: [[Antiphod]] -> Parser [[Antiphod]]
pRooms [r1,r2,r3,r4] =
  (pAntiphod >>= \a -> pRooms [r2,r3,r4,r1++[a]]) <|> (pSkip >> return [r1,r2,r3,r4])

pInput :: Parser Burrow
pInput = pRooms  [[],[],[],[]] >>= return . (take 11 (repeat E) :)

-- Part 1

-- first int is room number, second room position, third Hall position
data Move = ToHall Int Int Int | FromHall Int Int Int
  deriving (Eq,Show)

move :: Burrow -> Move -> Burrow
move b m@(ToHall r i h) =
  let a = mAnti b m
      newRoom = replace i E (room b r)
      newHall = replace h a (hall b)
      in replace 0 newHall (replace r newRoom b)
move b m@(FromHall r i h) =
  let a = mAnti b m
      newHall = replace h E (hall b)
      newRoom = replace i a (room b r)
      in replace 0 newHall (replace r newRoom b)

mDist :: Move -> Int
mDist (ToHall r p h) = p+1 + abs (2*r-h)
mDist (FromHall r p h) = p+1 + abs (2*r-h)

energy :: Antiphod -> Move -> Int
energy a m = aEn a * mDist m
  where aEn A = 1
        aEn B = 10
        aEn C = 100
        aEn D = 1000

mAnti :: Burrow -> Move -> Antiphod
mAnti b (ToHall i p h) = head (filter (/= E) (b!!i))
mAnti b (FromHall i p h) = (hall b)!!h

mEnergy :: Burrow -> Move -> Int
mEnergy b m = energy (mAnti b m) m

{-
-- top inhabited position in a room
inRoom :: [Antiphod] -> Maybe Int
inRoom r = let n = length (takeWhile (==E) r)
           in if n < length r then Just n else Nothing 
-}

-- free places in the Hall around a position
freeHall :: [Antiphod] -> Int -> [Int]
freeHall h i = (fBefore (i-1) ++ fAfter (i+1))
  where fBefore j = if j<0 || j>=length h || h!!j /= E then [] else j:fBefore (j-1)
        fAfter j = if j<0 || j>=length h || h!!j /= E then [] else j:fAfter (j+1)

roomAnti :: Int -> Antiphod
roomAnti 1 = A
roomAnti 2 = B
roomAnti 3 = C
roomAnti 4 = D

antiRoom :: Antiphod -> Int
antiRoom A = 1
antiRoom B = 2
antiRoom C = 3
antiRoom D = 4


firstA :: [Antiphod] -> Int
firstA = length . takeWhile (==E)

freePlace :: [Antiphod] -> Int -> [Int]
freePlace r i = let (es,as) = span (==E) r
                in if all (==roomAnti i) as && length es > 0
                   then [length es -1]
                   else []
{-
freePlace [E,E] _ = [1]
freePlace [E,a] i = if a == roomAnti i then [0] else []
freePlace _     _ = []
-}

-- possible movements from a room
fromRoom :: Burrow -> Int -> [Move]
fromRoom b i =
  let r = b!!i
      ra = roomAnti i
      j = firstA r
      a = r!!j
  in if all (`elem` [E,ra]) r then []
     else map (ToHall i j) (freeHall (hall b) (2*i) \\ [2,4,6,8])    -- [2*i,2*(antiRoom a)])

{-
  case room b i of
  [E,E] -> []
  [E,a] -> if a == roomAnti i then [] 
           else map (ToHall i 1) (freeHall (hall b) (2*i) \\ [2,4,6,8])
  as -> if all (== roomAnti i) as then []
        else map (ToHall i 0) (freeHall (hall b) (2*i) \\ [2,4,6,8])
-}
  
goodRoom :: [Antiphod] -> Int -> Bool
goodRoom r i = all (== roomAnti i) r


fromHall :: Burrow -> Int -> Int -> [Move]
fromHall b h i =
       if 2*i `elem` (freeHall (hall b) h)
       then map (\j -> FromHall i j h) (freePlace (room b i) i)
       else []

-- possible movements from the hall to a room
toRoom :: Burrow -> Int -> [Move]
toRoom b h = case (hall b)!!h of
  E -> []
  A -> fromHall b h 1
  B -> fromHall b h 2
  C -> fromHall b h 3
  D -> fromHall b h 4
  
moves :: Burrow -> [Move]
moves b = case concat (map (toRoom b) [0..10]) of
  (m:_) -> [m]
  _ -> concat $ map (fromRoom b) [1,2,3,4]

finalB :: Burrow
finalB = [ take 11 (repeat E), [A,A], [B,B], [C,C], [D,D] ]

minEnergy :: Burrow -> Int
minEnergy b =
  if b == finalB
  then 0
  else minimumBound (10^10) $ map (\m -> mEnergy b m + minEnergy (move b m)) (moves b)

burrowGraph :: GraphF Burrow
burrowGraph b = map (\m -> (move b m, mEnergy b m)) (moves b)

part1 :: Burrow -> Int
part1 b = dijkstra burrowGraph b finalB

-- Part 2

fullB :: Burrow -> Burrow
fullB b = [b!!0, ins (b!!1) [D,D],
                 ins (b!!2) [C,B],
                 ins (b!!3) [B,A],
                 ins (b!!4) [A,C] ]
  where ins [x0,x1] [y0,y1] = [x0,y0,y1,x1]

fullFinalB :: Burrow
fullFinalB = [ take 11 (repeat E), [A,A,A,A], [B,B,B,B], [C,C,C,C], [D,D,D,D] ]


part2 :: Burrow -> Int
part2 b = dijkstra burrowGraph (fullB b) fullFinalB

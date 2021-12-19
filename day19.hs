-- Advent of Code 2021, day 19

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
  ebs <- readFile "beacons" >>= return . parseAll (many pBeacon)
  let xs = parseAll pInput input
      sbs = nub $ matchAll [] xs
      scanners = map fst sbs
      beacons = concat $ map snd sbs
  putStrLn ("Part 1: " ++ show (length beacons))
  putStrLn ("Part 2: " ++ show (maxDist scanners))

-- Parsing the input

type Coordinates = (Int,Int,Int)
type Scanner = [Coordinates]

pBeacon :: Parser Coordinates
pBeacon = do x <- integer
             symbol ","
             y <- integer
             symbol ","
             z <- integer
             return (x,y,z)

pSHeader :: Parser Int
pSHeader = do symbol "--- scanner"
              n <- natural
              symbol "---"
              return n
              
pScanner :: Parser Scanner
pScanner = do pSHeader
              many pBeacon

pInput :: Parser [Scanner]
pInput = many pScanner

-- Part 1

-- translating a beacon
bTrans :: Coordinates -> Coordinates -> Coordinates
bTrans (x,y,z) (u,v,w) = (x+u,y+v,z+w)

-- Shift to bring the second beacon to the position of the first
bShift :: Coordinates -> Coordinates -> Coordinates
bShift (x,y,z) (u,v,w) = (x-u,y-v,z-w)


-- translating a scanner
sTrans :: Coordinates -> Scanner -> Scanner
sTrans c = map (bTrans c)

-- number of commont beacons of two scanners (without translations/rotations)
sMatchStatic :: Scanner -> Scanner -> [Coordinates]
sMatchStatic s1 s2 = intersect s1 s2

-- list of translations that make 12 beacons match
sMatchTrans :: Scanner -> Scanner -> [Coordinates]
sMatchTrans s1 s2 = [ bShift b1 b2 | b1 <- s1, b2 <- s2,
                                     length (sMatchStatic s1 (sTrans (bShift b1 b2) s2)) >= 12]

type Rotation = (Int,Int,Int) -- rotation around each of the three axes

-- rotating a beacon
bRot :: Rotation -> Coordinates -> Coordinates
bRot (rx,ry,rz) = nIter rotX rx . nIter rotY ry . nIter rotZ rz
  where rotX (x,y,z) = (x,-z,y)
        rotY (x,y,z) = (z,y,-x)
        rotZ (x,y,z) = (-y,x,z)

sRot :: Rotation -> Scanner -> Scanner
sRot r = map (bRot r)

-- all possible rotations
allRots :: [Rotation]
allRots = [(rx,ry,rz) | rx <- [0..3], ry <- [0..3], rz <- [0..3]]


sMatch :: Scanner -> Scanner -> [(Rotation,Coordinates)]
sMatch s1 s2 = concat [ map (\t -> (r,t)) $ sMatchTrans s1 (sRot r s2) |
                        r <- allRots ]

matchSplit :: Scanner -> [Scanner] -> ([(Coordinates,Scanner)],[Scanner])
matchSplit s1 [] = ([],[])
matchSplit s1 (s2:s2s) = let (ys,ns) = matchSplit s1 s2s in
  case sMatch s1 s2 of
    [] -> (ys,s2:ns)
    (r,t):_ -> ((t,sTrans t $ sRot r s2):ys,ns)

matchAll :: [(Coordinates,Scanner)] -> [Scanner] -> [(Coordinates,Scanner)]
matchAll [] [] = []
matchAll [] (s2:s2s) = matchAll [((0,0,0),s2)] s2s
matchAll ((t1,s1):s1s) s2s = let (ys,ns) = matchSplit s1 s2s in
  (t1,s1) : matchAll (s1s++ys) ns

{-
beacons :: [Scanner] -> [Coordinates]
beacons = nub . concat . matchAll []
-}

-- Part 2

-- Manhattan distance
mDist :: Coordinates -> Coordinates -> Int
mDist (x1,y1,z1) (x2,y2,z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

maxDist :: [Coordinates] -> Int
maxDist cs = maximum [ mDist p1 p2 | p1 <- cs, p2 <- cs]

part2 :: [Scanner] -> Int
part2 _ = 2

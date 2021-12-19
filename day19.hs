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
  let xs = parseAll pInput input
      s1 = xs!!0
      s2 = xs!!1
      (r,t) = head (sMatch s1 s2)
      s2' = sTrans t $ sRot r s2
  putStrLn (show (intersect s1 s2'))
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

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

matchSplit :: Scanner -> [Scanner] -> ([Scanner],[Scanner])
matchSplit s1 [] = ([],[])
matchSplit s1 (s2:s2s) = let (ys,ns) = matchSplit s1 s2s in
  case sMatch s1 s2 of
    [] -> (ys,s2:ns)
    (r,t):_ -> ((sTrans t $ sRot r s2):ys,ns)

matchAll :: [Scanner] -> [Scanner] -> [Scanner]
matchAll [] [] = []
matchAll [] (s2:s2s) = matchAll [s2] s2s
matchAll (s1:s1s) s2s = let (ys,ns) = matchSplit s1 s2s in
  s1 : matchAll (s1s++ys) ns


part1 :: [Scanner] -> Int
part1 ss = length $ matchAll [] ss

-- Part 2

part2 :: [Scanner] -> Int
part2 _ = 2

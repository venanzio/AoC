-- Advent of Code 2025, day 8
--  Venanzio Capretta

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
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type Point3D = (Int,Int,Int)
  
pData :: Parser Point3D
pData = do x <- natural
           y <- natural
           z <- natural
           return (x,y,z)

pInput :: Parser [Point3D]
pInput = some pData

-- Part 1

dist :: Point3D -> Point3D -> Int
dist (x0,y0,z0) (x1,y1,z1) = (x0-x1)^2 + (y0-y1)^2 + (z0-z1)^2

closest :: Point3D -> [Point3D] -> (Point3D,Int)
closest p [] = ((1000,1000,1000),1000000000)
closest p (p0:ps) = let d0 = dist p p0
                        (p1,d1) = closest p ps
                    in if d0 < d1 then (p0,d0) else (p1,d1)

closePairs :: [Point3D] -> [(Point3D,Point3D)]
closePairs ps = map (\(p0,(p1,_)) -> (p0,p1)) $
  sortBy (\(_,(_,d0)) (_,(_,d1)) -> compare d0 d1) (cpDist ps) where
    cpDist [] = []
    cpDist [p] = []
    cpDist (p:ps) = (p, closest p ps) : cpDist ps

type Circuit = M.Map Point3D [Point3D]

noConnect :: [Point3D] -> Circuit
noConnect = M.fromList . map (\p -> (p,[p]))

connect :: Point3D -> Point3D -> Circuit -> Circuit
connect p0 p1 circuits =
  M.update (\c -> Just (union c [p1])) p0 $
    M.update (\c -> Just (union c [p0])) p1 circuits

{-
connect :: Int -> [(Point3D,Point3D)] -> [[Point3D]]
connect 0 _ _ = []
connect n (p0,p1) ps = undefined
-}

part1 :: [Point3D] -> Int
part1 _ = 1

-- Part 2

part2 :: [Point3D] -> Int
part2 _ = 2

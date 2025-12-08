-- Advent of Code 2025, day 8
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
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

maxPoint :: Point3D
maxPoint = (1000,1000,1000)

pData :: Parser Point3D
pData = do x <- natural
           symbol ","
           y <- natural
           symbol ","
           z <- natural
           return (x,y,z)

pInput :: Parser [Point3D]
pInput = some pData

-- Part 1

dist :: Point3D -> Point3D -> Int
dist (x0,y0,z0) (x1,y1,z1) = (x0-x1)^2 + (y0-y1)^2 + (z0-z1)^2

connections :: [Point3D] -> [(Point3D,Point3D)]
connections ps = sortOn (\(p0,p1) -> dist p0 p1) (allPairs ps)

type Circuit = M.Map Point3D [Point3D]

noConnect :: [Point3D] -> Circuit
noConnect = M.fromList . map (\p -> (p,[p]))

connect :: Point3D -> Point3D -> Circuit -> Circuit
connect p0 p1 circuits = foldr (\p -> M.insert p circuit) circuits circuit
  where circuit = union (M.findWithDefault [] p0 circuits)
                        (M.findWithDefault [] p1 circuits)
                  
deleteCircuit :: [Point3D] -> Circuit -> Circuit
deleteCircuit [] circuits = circuits
deleteCircuit (c:cs) circuits = deleteCircuit cs (M.delete c circuits)

connectAll :: [(Point3D,Point3D)] -> Circuit -> Circuit
connectAll ps circuits = foldl (\c (p0,p1) -> connect p0 p1 c) circuits ps

allCircuits :: [Point3D] -> Circuit -> [[Point3D]]
allCircuits [] circuits = []
allCircuits (p:ps) circuits = circuit : allCircuits (ps\\circuit) circuits' where
  circuit = M.findWithDefault [] p circuits
  circuits' = deleteCircuit circuit circuits

part1 :: [Point3D] -> Int
part1 ps = product (take 3 lengths) where
  circuits = connectAll (take 1000 (connections ps)) (noConnect ps)
  lengths = sortBy (flip compare) $ map length $ allCircuits ps circuits

-- Part 2

oneCircuit :: [(Point3D,Point3D)] -> Circuit -> (Point3D,Point3D)
oneCircuit [] circuits = (maxPoint,maxPoint)
oneCircuit ((p0,p1):cs) circuits =
  if length (M.findWithDefault [] p0 circuits') == M.size circuits'
    then (p0,p1)
    else oneCircuit cs circuits'
  where circuits' = connect p0 p1 circuits

part2 :: [Point3D] -> Int
part2 ps = x0 * x1 where
  ((x0,_,_),(x1,_,_)) = oneCircuit (connections ps) (noConnect ps)

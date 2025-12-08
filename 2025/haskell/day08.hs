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
  putStrLn (show $ take 5 $ connections xs)
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

{-
closest :: Point3D -> [Point3D] -> (Point3D,Int)
closest p [] = ((1000,1000,1000),1000000000)
closest p (p0:ps) = let d0 = dist p p0
                        (p1,d1) = closest p ps
                    in if d0 < d1 then (p0,d0) else (p1,d1)
                                                    
closePair ::  [Point3D] -> Circuit -> (Point3D,Point3D)
closePair ps circuits = (\(p0,(p1,_)) -> (p0,p1)) $
  minimumBy (\(_,(_,d0)) (_,(_,d1)) -> compare d0 d1) $
  map (\p -> (p, closest p (ps\\M.findWithDefault [] p circuits))) ps
-}

type Circuit = M.Map Point3D [Point3D]

noConnect :: [Point3D] -> Circuit
noConnect = M.fromList . map (\p -> (p,[p]))

{-
connect :: Point3D -> Point3D -> Circuit -> Circuit
connect p0 p1 circuits =
  M.update (\c -> Just (union c [p1])) p0 $
    M.update (\c -> Just (union c [p0])) p1 circuits

connectClose :: [Point3D] -> Circuit -> Circuit
connectClose ps circuits = connect p0 p1 circuits where
  (p0,p1) = closePair ps circuits

connectAll :: Int -> [Point3D] -> Circuit
connectAll 0 ps = noConnect ps
connectAll n ps = connectClose ps (connectAll (n-1) ps)

largestCircuit :: Circuit -> [Point3D]
largestCircuit = maximumBy (\c0 c1 -> compare (length c0) (length c1)) . M.elems

deleteCircuit :: [Point3D] -> Circuit -> Circuit
deleteCircuit [] circuits = circuits
deleteCircuit (c:cs) circuits = deleteCircuit cs (M.delete c circuits)

fullCircuit :: [Point3D] -> Circuit -> ([Point3D], Circuit)
fullCircuit [] circuits = ([],circuits)
fullCircuit (p:ps) circuits =
  (nub (p:connected++circuit), circuits2) where
    connected = M.findWithDefault [] p circuits
    circuits1 = M.delete p circuits
    (circuit,circuits2) = fullCircuit (nub $ connected++ps) circuits1

allCircuits :: [Point3D] -> Circuit -> [[Point3D]]
allCircuits [] _ = []
allCircuits (p:ps) circuits = circuit : allCircuits (ps\\circuit) circuits' where
  (circuit, circuits') = fullCircuit [p] circuits
  
part1 :: [Point3D] -> Int
part1 ps = length (cs!!0) * length (cs!!1) * length (cs!!2) where
  circuits = connectAll 1000 ps
  cs = sortBy (\l1 l2 -> compare (length l2) (length l1)) $ allCircuits ps circuits
-}

connectAll :: [(Point3D,Point3D)] -> Circuit -> Circuit
connectAll ps circuits = foldl (\c (p0,p1) -> connect p0 p1 c) circuits ps

allCircuits :: [Poit3D] -> Circuit -> [[Point3D]]
allCircuits ps circuits = if null circuits then []
  else undefined

part1 :: [Point3D] -> Int
part1 ps = product (take 3 lengths) where
  circuits = connectAll (take 10 (connections ps)) (noConnect ps)
  lengths = sortBy (\l1 l2 -> compare l2 l1) $ map length $ M.elems circuits

-- Part 2

connections :: [Point3D] -> [(Point3D,Point3D)]
connections ps = sortOn (\(p0,p1) -> dist p0 p1) (allPairs ps)

connect :: Point3D -> Point3D -> Circuit -> Circuit
connect p0 p1 circuits = foldr (\p -> M.insert p circuit) circuits circuit
  where circuit = union (M.findWithDefault [] p0 circuits)
                        (M.findWithDefault [] p1 circuits)

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

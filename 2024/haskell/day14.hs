-- Advent of Code 2024, day 14
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List

import FunParser
import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let rs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 rs))
--  let (n,rs0) = countSecs rs
--  putStrLn (showPoints '#' (map fst rs0))
  putStrLn ("Part 2: " ++ show (part2 rs))

-- Parsing the input

pIntPair :: Parser Point
pIntPair = do x <- integer
              symbol ","
              y <- integer
              return (x,y)

pData :: Parser (Point,Direction)
pData = do symbol "p="
           p <- pIntPair
           symbol "v="
           v <- pIntPair
           return (p,v)

pInput :: Parser [(Point,Direction)]
pInput = pLines pData

-- Part 1

sizeX = 101
sizeY = 103
midX = sizeX `div` 2
midY = sizeY `div` 2

move :: Int -> (Point,Direction) -> Point
move secs ((x,y),(vx,vy)) = ((x + vx*secs) `mod` sizeX,
                             (y + vy*secs) `mod` sizeY) 

quadrant :: Int -> [Point] -> Int
quadrant 1 = length . filter (\(x,y) -> x < midX && y < midY)
quadrant 2 = length . filter (\(x,y) -> x > midX && y < midY) 
quadrant 3 = length . filter (\(x,y) -> x < midX && y > midY)
quadrant 4 = length . filter (\(x,y) -> x > midX && y > midY)
quadrant n = error ("quadrant " ++  show n ++ " doesn't exist")

part1 :: [(Point,Direction)] -> Int
part1 rs = product $  map (flip quadrant $ map (move 100) rs) [1,2,3,4]

-- Part 2

symmetric :: [Point] -> Bool
symmetric ps = all (\(x,y) -> (sizeX - x - 1,y) `elem` ps) ps

cluster :: [Point] ->  Point -> (Int,[Point])
cluster ps p = clusterAux 0 ps [p]

clusterAux :: Int -> [Point] ->  [Point] -> (Int,[Point])
clusterAux n ps [] = (n,ps)
clusterAux n ps (p:rs) = clusterAux (n+1) ps0 (qs++rs)
  where qs = [q | q <- neighboursHV p, q `elem` ps]
        ps0 = ps \\ qs

clusters :: [Point] -> [Int]
clusters [] = []
clusters (p:ps) = let (n,ps0) = cluster ps p
                  in n : clusters ps0

largestCluster :: [Point] -> Int
largestCluster = maximum . clusters

wrap :: Point -> Point
wrap (x,y) = (x `mod` sizeX, y `mod` sizeY)

step :: (Point,Direction) -> (Point,Direction)
step (p,v) = (wrap $ pMove p v,v)

bigCluster :: [Point] -> Bool
bigCluster ps = largestCluster ps > 50

countSecs :: [(Point,Direction)] -> (Int,[(Point,Direction)])
countSecs =  iterSat (bigCluster . map fst) (map step) 

part2 :: [(Point,Direction)] -> Int
part2 = fst. countSecs

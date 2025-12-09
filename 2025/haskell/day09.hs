-- Advent of Code 2025, day 9
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

pData :: Parser Point
pData = do x <- natural
           symbol ","
           y <- natural
           return (x,y)

pInput :: Parser [Point]
pInput = some pData

-- Part 1


rectArea :: Point -> Point -> Int
rectArea p0 p1 = (1 + max (pX p0) (pX p1) - min (pX p0) (pX p1)) *
                 (1 + max (pY p0) (pY p1) - min (pY p0) (pY p1))

part1 :: [Point] -> Int
part1 ps = maximum [rectArea p0 p1 | p0 <- ps, p1 <- ps]

-- Part 2

vertical :: (Point,Point) -> Bool
vertical (p0,p1) = pX p0 == pX p1

horizontal :: (Point,Point) -> Bool
horizontal (p0,p1) = pY p0 == pY p1

pVInside :: Point -> (Point,Point) -> Bool
pVInside p (q0,q1) = (min (pY q0) (pY q1) <= pY p) && (max (pY q0) (pY q1) >= pY p)

vInside :: (Point,Point) -> (Point,Point) -> Bool
vInside (p0,p1) ql = pVInside p0 ql && pVInside p1 ql

pHInside :: Point -> (Point,Point) -> Bool
pHInside p (q0,q1) = (min (pX q0) (pX q1) <= pX p) && (max (pX q0) (pX q1) >= pX p)

hInside :: (Point,Point) -> (Point,Point) -> Bool
hInside (p0,p1) ql = pHInside p0 ql && pHInside p1 ql

vhCross :: (Point,Point) -> (Point,Point) -> Bool
vhCross p@(p0,p1) q@(q0,q1) =
  (min (pX q0) (pX q1)) < pX p0 && (max (pX q0) (pX q1)) > pX p0 &&
  (min (pY p0) (pY p1)) < pY q0 && (max (pY p0) (pY p1)) > pY q0

hvCross :: (Point,Point) -> (Point,Point) -> Bool
hvCross = flip vhCross

-- Crossing lines, or collinear and first not contain in second
lCross :: (Point,Point) -> (Point,Point) -> Bool
lCross p q 
  | vertical p && horizontal q = vhCross p q
  | horizontal p && vertical q = hvCross p q
  | vertical p && vertical q = not $ vInside p q
  | horizontal p && horizontal q = not $ hInside p q

-- Point is inside a rectangle
pInRect :: Point -> (Point,Point) -> Bool
pInRect p (q0,q1) = pInside p (minX,minY) (maxX,maxY) where
  minX = min (pX q0) (pX q1)
  maxX = max (pX q0) (pX q1)
  minY = min (pY q0) (pY q1)
  maxY = max (pY q0) (pY q1)

rectSides :: (Point,Point) -> [(Point,Point)]
rectSides (p0,p1) = [((minX,minY),(maxX,minY)),
                     ((maxX,minY),(maxX,maxY)),
                     ((maxX,maxY),(minX,maxY)),
                     ((minX,maxY),(minX,minY))]
  where   minX = min (pX p0) (pX p1)
          maxX = max (pX p0) (pX p1)
          minY = min (pY p0) (pY p1)
          maxY = max (pY p0) (pY p1)

rectInPolygon :: (Point,Point) -> [(Point,Point)] -> Bool
rectInPolygon p edges = not (pInRect (fst (head edges)) p) &&
  and [not $ lCross pl ql | pl <- rectSides p, ql <- edges]


part2 :: [Point] -> Int
part2 _ = 2

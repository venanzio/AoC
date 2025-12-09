-- Advent of Code 2025, day 9
--  Venanzio Capretta

module Main where

import System.Environment
import Control.Applicative
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

rectArea :: (Point,Point) -> Int
rectArea (p0,p1) = (1 + max (pX p0) (pX p1) - min (pX p0) (pX p1)) *
                   (1 + max (pY p0) (pY p1) - min (pY p0) (pY p1))

part1 :: [Point] -> Int
part1 ps = maximum [rectArea (p0,p1) | p0 <- ps, p1 <- ps]

-- Part 2

vertical :: (Point,Point) -> Bool
vertical (p0,p1) = pX p0 == pX p1

horizontal :: (Point,Point) -> Bool
horizontal (p0,p1) = pY p0 == pY p1

-- Point is inside a rectangle
pInRect :: Point -> (Point,Point) -> Bool
pInRect p@(x,y) ((x0,y0),(x1,y1)) = x0 < x && x < x1 && y0 < y && y < y1

-- Point outside a rectangle
pOutRect :: Point -> (Point,Point) -> Bool
pOutRect p r = not $ pInRect p r

-- A point is on a vertical or horizontal line
inLine :: Point -> (Point,Point) -> Bool
inLine p@(x,y) l@((x0,y0),(x1,y1)) =
  vertical l && x==x0 &&
     (min y0 y1) <= y && (max y0 y1) >= y ||
  horizontal l && y==y0 &&
     (min x0 x1) <= x && (max x0 x1) >= x   

-- Winding number (if point not on the perimeter)
wind :: Point -> [Point] -> Int
wind p poly = windAcc 0 (last poly) poly where
  windAcc n prev [] = n
  windAcc n prec (q:qs)
    | pY prec >= pY p = windAcc n q qs
    | pX prec <= pX p = if pX q > pX p then windAcc (n-1) q qs
                                       else windAcc n q qs
    | pX prec > pX p = if pX q <= pX p then windAcc (n+1) q qs
                                       else windAcc n q qs
-- Point is inside the polygon
pInPoly :: Point -> [Point] -> Bool
pInPoly p poly = any (inLine p) edges || wind p poly /= 0
  where edges = polyEdges poly
                                            
-- a line crosses a rectangle
crossRect :: (Point,Point) -> (Point,Point) -> Bool
crossRect l@((l0X,l0Y),(l1X,l1Y)) r@((x0,y0),(x1,y1)) =
  vertical l   && x0 < l0X && l0X < x1 && lYmin <= y0 && lYmax >= y1 ||
  horizontal l && y0 < l0Y && l0Y < y1 && lXmin <= x0 && lXmax >= x1
  where lYmin = min l0Y l1Y
        lYmax = max l0Y l1Y
        lXmin = min l0X l1X
        lXmax = max l0X l1X

rectCenter :: (Point,Point) -> Point
rectCenter ((x0,y0),(x1,y1)) = ((x0 + x1) `div` 2, (y0 + y1) `div` 2)
  
{- A rectangle is inside the polygon if:
   * all polygon points are outside the rectangle
   * the center of the rectangle is inside the polygon
   * no polygon edge crosses the rectangle
  Still insufficient: wrong for flat rectangles and adjecent edges
-}
rectInPolygon :: (Point,Point) -> [Point] -> Bool
rectInPolygon r@(p0,p1) poly =
  all (\p -> pOutRect p r) poly &&
  pInPoly (rectCenter r) poly &&
  and [not $ crossRect l r | l <- polyEdges poly]

polyEdges :: [Point] -> [(Point,Point)]
polyEdges ps = (last ps,head ps) : peAux ps where
  peAux [p] = []
  peAux (p0:p1:ps) = (p0,p1) : peAux (p1:ps)

rectangles :: [Point] -> [(Point,Point)]
rectangles [] = []
rectangles [p] = []
rectangles (p:ps) = map (\q -> corners p q) ps ++  rectangles ps where
  corners (x0,y0) (x1,y1) = ((min x0 x1, min y0 y1), (max x0 x1, max y0 y1))

part2 :: [Point] -> Int
part2 ps = rectArea (head rectInside) where 
  rs = reverse $ sortOn rectArea $ rectangles ps
  rectInside = filter (\r -> rectInPolygon r ps) rs

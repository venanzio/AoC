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
{-
pVInside :: Point -> (Point,Point) -> Bool
pVInside p (q0,q1) = pX p == pX q0 &&
                     (min (pY q0) (pY q1) <= pY p) && (max (pY q0) (pY q1) >= pY p)

vInside :: (Point,Point) -> (Point,Point) -> Bool
vInside (p0,p1) ql = pVInside p0 ql && pVInside p1 ql

pHInside :: Point -> (Point,Point) -> Bool
pHInside p (q0,q1) = pY p == pY q0 &&
                     (min (pX q0) (pX q1) <= pX p) && (max (pX q0) (pX q1) >= pX p)

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
  | vertical p && vertical q = pX (fst p) == pX (fst q) && not (vInside p q)
  | horizontal p && horizontal q =  pY (fst p) == pY (fst q) && not (hInside p q)

-}

-- Point is inside a rectangle
pInRect :: Point -> (Point,Point) -> Bool
pInRect p@(x,y) (q0,q1) = minX < x && x < maxX && minY < y && y < maxY where
  minX = min (pX q0) (pX q1)
  maxX = max (pX q0) (pX q1)
  minY = min (pY q0) (pY q1)
  maxY = max (pY q0) (pY q1)

-- Point outside a rectangle
pOutRect :: Point -> (Point,Point) -> Bool
pOutRect p r = not $ pInRect p r

-- Point is inside the polygon
pInPoly :: Point -> [Point] -> Bool
pInPoly p poly = any (inLine p) edges || wind p poly /= 0
  where edges = polyEdges poly

-- A point is on a vertical or horizontal line
inLine :: Point -> (Point,Point) -> Bool
inLine p l@(l0,l1) =
  vertical l && (pX p)==(pX l0) &&
     (min (pY l0) (pY l1)) <= (pY p) && (max (pY l0) (pY l1)) >= (pY p) ||
  horizontal l && (pY p)==(pY l0) &&
     (min (pX l0) (pX l1)) <= (pX p) && (max (pX l0) (pX l1)) >= (pX p)   

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


-- a horizontal line crosses above a point
cross :: Point -> (Point,Point) -> Bool
cross (px,py) ((x0,y),(x1,_)) = y > py && undefined

-- a line crosses a rectangle
crossRect :: (Point,Point) -> (Point,Point) -> Bool
crossRect l@((l0X,l0Y),(l1X,l1Y)) r@(q0,q1) =
  vertical l && minX < l0X && l0X < maxX && lYmin <= minY && lYmax >= maxY ||
  horizontal l && minY < l0Y && l0Y < maxY && lXmin <= minX && lXmax >= maxX
  where minX = min (pX q0) (pX q1)
        maxX = max (pX q0) (pX q1)
        minY = min (pY q0) (pY q1)
        maxY = max (pY q0) (pY q1)
        lYmin = min l0Y l1Y
        lYmax = max l0Y l1Y
        lXmin = min l0X l1X
        lXmax = max l0X l1X
{-        
rectSides :: (Point,Point) -> [(Point,Point)]
rectSides (p0,p1) = [((minX,minY),(maxX,minY)),
                     ((maxX,minY),(maxX,maxY)),
                     ((maxX,maxY),(minX,maxY)),
                     ((minX,maxY),(minX,minY))]
  where   minX = min (pX p0) (pX p1)
          maxX = max (pX p0) (pX p1)
          minY = min (pY p0) (pY p1)
          maxY = max (pY p0) (pY p1)
-}

rectCenter :: (Point,Point) -> Point
rectCenter (p,q) = (pX p + ((pX q - pX p) `div` 2),
                    pY p + ((pY q - pY p) `div` 2))

{- A rectangle is inside the polygon if:
   * all polygon points are outside the rectangle
   * the center of the rectangle is inside the polygon
   * no polygon edge crosses the rectangle
  Still insufficient: wrong for flat rectangles
-}
rectInPolygon :: (Point,Point) -> [Point] -> Bool
rectInPolygon r@(p0,p1) poly =
  all (\p -> pOutRect p r) poly &&
  pInPoly (rectCenter r) poly &&
  and [not $ crossRect l r | l <- polyEdges poly]
  

{-
rectInPolygon p edges = not (pInRect (fst (head edges)) p) &&
  and [not $ lCross pl ql | pl <- rectSides p, ql <- edges]
-}


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
part2 ps = maximum $ map rectArea rectInside where
  rs = rectangles ps
  rectInside = filter (\r -> rectInPolygon r ps) rs

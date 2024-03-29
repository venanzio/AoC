-- Advent of Code 2023, day 10
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import qualified Data.Map as M
import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let m = length ((lines input)!!0)
      n = length (lines input)
      area = pArea input
      field = areaField area
      start = areaStart area
      l = loop field start
  putStrLn ("Part 1: " ++ show (part1 l))
  putStrLn ("Part 2: " ++ show (part2 m n l))

  {- Alternative solution using the shoelace formula:
  putStrLn ("Part 2: " ++ show (shoelace l - length l `div` 2))
     I don't know why, this gives a result that is off by 1
  -}
  
-- Parsing the input

type Position = (Int,Int)
type Field = M.Map Position [Position]

north :: Position -> Position
north (x,y) = (x,y-1)

south :: Position -> Position
south (x,y) = (x,y+1)

west :: Position -> Position
west (x,y) = (x-1,y)

east :: Position -> Position
east (x,y) = (x+1,y)

  
pArea :: String -> M.Map (Int,Int) Char
pArea = matrixMap (0,0) . lines

neighbours :: M.Map (Int,Int) Char -> Position -> [Position]
neighbours area p = [q | (q,ds) <- [(north p,"|7F"), (south p,"|LJ"), (west p,"-LF"), (east p,"-J7")], M.member q area, (area M.! q) `elem` ds]


areaField :: M.Map (Int,Int) Char -> Field
areaField area = M.mapMaybeWithKey pipe area where
  pipe p c = case c of
    '|' -> Just [north p, south p]
    '-' -> Just [west p, east p]
    'L' -> Just [north p, east p]
    'J' -> Just [north p, west p]
    '7' -> Just [south p, west p]
    'F' -> Just [south p, east p]
    'S' -> Just (neighbours area p)
    _ -> Nothing

areaStart :: M.Map (Int,Int) Char -> Position
areaStart area = head $ M.keys $  M.filter (=='S') area

-- Part 1

move :: Field -> Position -> Position -> Position
move field previous p = head $ delete previous (field M.! p)

loop :: Field -> Position -> [Position]
loop field p = loopFrom p (move field p p) where
  loopFrom q0 q1 = if q1 == p then [q0] else q0 : loopFrom q1 (move field q0 q1)

part1 :: [Position] -> Int
part1 l = length l `div` 2

-- Part 2

winding :: Position -> [Position] -> Int
winding (x0,y0) l = winDir 0 (lastX x0) l where
  winDir w px [] = w
  winDir w px qs@((x1,y1):qs1) =
    if x0==x1 && y0>y1
    then let (nx,qs') = nextXL qs
         in winDir (w + (nx-px) `div` 2) x1 qs'
    else winDir w x1 qs1

  lastX x = head $ filter (/=x) $ reverse $ map fst l

  nextXL [] = (fst $ head l, [])
  nextXL (qs@((x1,y1):qs')) = if x1 == x0 then nextXL qs' else (x1,qs)
    
enclosed :: [Position] -> [Position] -> [Position]
enclosed ground l = filter (\p -> winding p l /= 0) (ground \\ l)

allPos :: Int -> Int -> [Position]
allPos m n = [(x,y) | x <- [0..m-1], y <- [0..n-1]]

part2 ::  Int -> Int -> [Position] -> Int
part2 m n l = length $ enclosed (allPos m n) l



-- Alternative solution: use the shoelace formula for the area of a polygon
shoelace :: [Position] -> Int
shoelace ps = sum [trapezoid p0 p1 | (p0,p1) <- consPairs ps] `div` 2

trapezoid :: Position -> Position -> Int
trapezoid (x0,y0) (x1,y1) = (y0+y1)*(x0-x1)

consPairs :: [a] -> [(a,a)]
consPairs xs = zip xs (tail xs ++ [head xs])

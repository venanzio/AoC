-- Advent of Code 2023, day 10
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
  let m = length ((lines input)!!0)
      n = length (lines input)
      area = pArea input
      field = areaField area
      start = areaStart area
      l = loop field start
  putStrLn ("Part 1: " ++ show (length l `div` 2))
  putStrLn ("Part 2: " ++ show (part2 m n l))

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
    'S' -> Just (neighbours area p) -- Just (filter (\n -> M.member n field) (neighbours p))
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

cardPoints :: Position -> [Position] -> String
cardPoints _ [] = []
cardPoints (x0,y0) ((x1,y1):ps)
  | x0==x1 || y0>y1 = 'N' : cardPoints (x0,y0) ps
  | x0==x1 || y0<y1 = 'S' : cardPoints (x0,y0) ps
  | x0>x1 || y1==y0 = 'W' : cardPoints (x0,y0) ps
  | x0<x1 || y1==y0 = 'E' : cardPoints (x0,y0) ps
  | otherwise       = cardPoints (x0,y0) ps

nextCard :: Char -> Char
nextCard 'N' = 'E'
nextCard 'E' = 'S'
nextCard 'S' = 'W'
nextCard 'W' = 'N'

delReps :: Eq a => [a] -> [a]
delReps [] = []
delReps [x] = [x]
delReps (x0:x1:xs) = if x0==x1 then delReps xs else x0 : delReps (x1:xs)
  
winding :: Position -> [Position] -> Int
winding p l = (length $ delReps $ cardPoints p l) `div` 4

enclosed :: [Position] -> [Position] -> Int
enclosed ground l = length $
  filter (\p -> winding p l > 0) (ground \\ l)

allPos :: Int -> Int -> [Position]
allPos m n = [(x,y) | x <- [0..m-1], y <- [0..n-1]]

part2 ::  Int -> Int -> [Position] -> Int
part2 m n l = enclosed (allPos m n) l


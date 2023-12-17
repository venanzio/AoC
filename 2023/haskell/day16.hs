-- Advent of Code 2023, day 16
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
  input <- readFile fileName >>= return.lines
  let contraption = pContraption input
  putStrLn ("Part 1: " ++ show (part1 contraption))
  putStrLn ("Part 2: " ++ show (part2 contraption))

-- Parsing the input

type Point = (Int,Int)
type Tile = Char -- must be one of /, \, -, |, .
type Contraption = (Int,Int,M.Map Point Tile)

pContraption :: [String] -> Contraption
pContraption ls =
  (length (ls!!0), length ls, mMapF (\c -> if c `elem` "/\\-|" then Just c else Nothing) ls)

-- Part 1

width :: Contraption -> Int
width (w,_,_) = w

height :: Contraption -> Int
height (_,h,_) = h

outBounds :: Point -> Contraption -> Bool
outBounds (x,y) (w,h,_) = x<0 || x>=w || y<0 || y>=h
  
pointContr :: Point -> Contraption -> Tile
pointContr point (_,_,c) =
  case M.lookup point c of
    Just t -> t
    Nothing -> '.'

data Direction = UpD | DownD | LeftD | RightD
  deriving (Eq,Show)
type Energized = M.Map Point [Direction]

move :: Point -> Direction -> Point
move (x,y) UpD = (x,y-1)
move (x,y) DownD = (x,y+1)
move (x,y) LeftD = (x-1,y)
move (x,y) RightD = (x+1,y)

changeDir :: Direction -> Tile -> [Direction]
changeDir UpD    '/' = [RightD]
changeDir DownD  '/' = [LeftD]
changeDir LeftD  '/' = [DownD]
changeDir RightD '/' = [UpD]
changeDir UpD    '\\' = [LeftD]
changeDir DownD  '\\' = [RightD]
changeDir LeftD  '\\' = [UpD]
changeDir RightD '\\' = [DownD]
changeDir UpD   '-' = [LeftD,RightD]
changeDir DownD '-' = [LeftD,RightD]
changeDir LeftD  '|' = [UpD,DownD]
changeDir RightD '|' = [UpD,DownD]
changeDir d _ = [d]

eDirections :: Point -> Energized -> [Direction]
eDirections p en = case M.lookup p en of
  Nothing -> []
  Just ds -> ds

beam :: Point -> Direction -> Energized -> Contraption -> ([(Point,Direction)],Energized)
beam p d en contraption | outBounds p contraption || d `elem` (eDirections p en) = ([],en)
beam p d en contraption =
 let ds = changeDir d (pointContr p contraption)
     ps = map (move p) ds
 in (zip ps ds, M.insert p (union [d] (eDirections p en)) en)

beams :: [(Point,Direction)] -> Energized -> Contraption -> ([(Point,Direction)],Energized)
beams [] en _ = ([],en)
beams pds en contraption = foldl beamAdd ([],en) pds
  where beamAdd (pds0,en0) (p,d) =
          let (pds1,en1) = beam p d en0 contraption
          in (pds1++pds0, en1)

energize :: ([(Point,Direction)],Energized) -> Contraption -> Energized
energize ([],en) _ = en
energize (pds,en) contraption = energize (beams pds en contraption) contraption

energized :: (Point,Direction) -> Contraption -> Int
energized pd = M.size . energize ([pd],M.empty)

part1 :: Contraption -> Int
part1 = energized ((0,0),RightD)

-- Part 2

part2 :: Contraption -> Int
part2 contraption = maximum $ map (flip energized contraption) $
  let w = width contraption
      h = height contraption
  in [((x,0)  , DownD) | x <- [0..w-1]] ++
     [((x,h-1), UpD)   | x <- [0..w-1]] ++
     [((0,y)  , DownD) | y <- [0..h-1]] ++
     [((w-1,y), UpD)   | y <- [0..h-1]]

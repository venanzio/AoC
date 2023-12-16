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

pointContr :: Point -> Contraption -> Tile
pointContr point contraption =
  case M.lookup point contraption of
    Just t -> t
    Nothing -> '.'

data Direction = UpD | DownD | LeftD | RightD
  deriving Eq
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
beam (x,y) _ _ (w,h,_) | x<0 || x>=w || y<0 || y>=h = undefined
beam p d en contraption =
  if d `elem` (eDirections p en)
  then ([],en)
  else let ds = changeDir d (pointContr p contraption)
           ps = map (move p) ds
       in (zip ps ds, M.insert p (union [d] (eDirections p en)) en)

beams :: [(Point,Direction)] -> Energized -> Contraption -> Energized
beams [] en _ = en
beams pds en contraption = snd $ foldl beamAdd ([],en) pds
  where beamAdd (pds0,en0) (p,d) =
          let (pds1,en1) = beam p d en0 contraption
          in (pds1++pds0, en1)
    
part1 :: Int -> Int -> Contraption -> Int
part1 width height contraption =
  length 

-- Part 2

part2 :: Int -> Int -> Contraption -> Int
part2 width height contraption = 2

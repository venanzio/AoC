module PuzzleInput where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M


puzzle :: String -> IO Int
puzzle fileName = do
  input <- readFile fileName
  let dim = readDim input
      dim' = final dim 6
  return (activeCount dim')


type Cube = (Int,Int,Int)
data Dimension = Dim { bottom  :: Cube,
                       top  :: Cube,
                       cells  :: M.Map Cube Bool
                     }
  -- The first two cubes are lower and upper corners

readDim :: String -> Dimension
readDim input =
  let ls = lines input
      x1 = length (head ls) - 1
      y1 = length ls -1
      cs = [ ((x,y,0), ls!!y!!x == '#') |
             x <- [0..x1], y <- [0..y1] ]
  in Dim { bottom = (0,0,0), top = (x1,y1,0), cells = M.fromList cs }

showDim :: Dimension -> String
showDim dim =
  "bottom: " ++ (show (bottom dim)) ++
  "\ntop: "  ++ (show (top dim)) ++ "\n\n" ++
  show (cells dim)

-- Part 1

status :: Dimension -> Cube -> Bool
status dim cube = case M.lookup cube (cells dim) of
  Just True -> True
  _         -> False

activate :: Cube -> Dimension -> Dimension
activate cube dim = dim {cells = M.insert cube True (cells dim)}

inactivate :: Cube -> Dimension -> Dimension
inactivate cube dim = dim {cells = M.insert cube False (cells dim)}


neighbours :: Cube -> [Cube]
neighbours (x,y,z) = [(x+i,y+j,z+k) | i<-[-1,0,1], j<-[-1,0,1], k<-[-1,0,1],
                                      (i,j,k) /= (0,0,0)]

nCount :: Cube -> Dimension -> Int
nCount cube dim = length $ filter (status dim) (neighbours cube)

change :: Cube -> Dimension -> Bool
change cube dim =
  let st = status dim cube
      count = nCount cube dim
  in (st && count `elem` [2,3]) || (not st && count == 3)

range :: Cube -> Cube -> [Cube]
range (x0,y0,z0) (x1,y1,z1) =
  [ (x,y,z) | x <- [x0..x1], y <- [y0..y1], z <- [z0..z1] ]

dimCycle :: Dimension -> Dimension
dimCycle dim =
  let (x0,y0,z0) = bottom dim
      (x1,y1,z1) = top dim
      bt = (x0-1,y0-1,z0-1)
      tp = (x1+1,y1+1,z1+1)
      cs = M.fromList  [(c, change c dim) |
                        c <- range bt tp ]
  in Dim { bottom = bt
         , top = tp
         , cells = cs
         }

activeCount :: Dimension -> Int
activeCount = M.foldr (\st n -> if st then n+1 else n) 0 . cells

final :: Dimension -> Int -> Dimension
final dim 0 = dim
final dim n = final (dimCycle dim) (n-1)


module PuzzleInput where

import System.Environment
import Data.List

import FunParser
import Control.Applicative

puzzle :: String -> IO Int
puzzle fileName = do
  input <- readFile fileName
  let rows = map readL $ words input
  -- return (coordSlope rows (3,1))  -- part 1
  return (coordSlope rows (1,1) * coordSlope rows (3,1) * coordSlope rows (5,1) * coordSlope rows (7,1) * coordSlope rows (1,2))  -- part 2


readL :: String -> [Bool]
readL [] = []
readL ('#':s) = True : readL s
readL ('.':s) = False : readL s
readL _ = error "unexpected character"

coordSlope :: [[Bool]] -> (Int,Int) -> Int
coordSlope rs (dx,dy) =
  coordS (length (head rs),length rs) (0,0) (dx,dy) 0 rs

coordS :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Int -> [[Bool]] -> Int
coordS (n,m) (x,y) (dx,dy) s rs =
  let y' = y + dy
      x' = if x+dx < n then x+dx else x+dx-n
  in if y' >= m
       then s
       else coordS (n,m) (x',y') (dx,dy) (s + if rs!!y'!!x' then 1 else 0) rs




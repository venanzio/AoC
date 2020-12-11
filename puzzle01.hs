module PuzzleInput where

import System.Environment
import Data.List

puzzle :: String -> IO (Int,Int,Int,Int)
puzzle fileName = do
  input <- readFile fileName
  let nums = map read $ words input :: [Int]
  return (threePuzzle nums)

-- Searching the list for two numbers that sum to 2020
twoPuzzle :: [Int] -> (Int,Int,Int)
twoPuzzle l = let sl = sort l
                  (x,y) = searchSum sl (reverse sl) 2020
              in (x,y,x*y)

-- Searching in two list, first increasing, second decreasing
searchSum :: [Int] -> [Int] -> Int -> (Int,Int)
searchSum xs@(x:xs') ys@(y:ys') z
  | x+y > z = searchSum xs ys' z
  | x+y < z = searchSum xs' ys z
  | otherwise  = (x,y)
searchSum _ _ _ = (0,0)


-- Searching for three numbers that add up to 2020
threePuzzle :: [Int] -> (Int,Int,Int,Int)
threePuzzle (w:ws) =
  let sl = sort ws
      (x,y) = searchSum sl (reverse sl) (2020 - w)
  in if (x,y) == (0,0) then threePuzzle ws
                       else (w,x,y,w*x*y)

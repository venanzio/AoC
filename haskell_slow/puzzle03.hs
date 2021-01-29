-- Advent of Code 2020, day 3

module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)
  
puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let rows = map readL $ lines input
  putStrLn ("Part 1: " ++ show (slopeTrees rows (3,1)))
  putStrLn ("Part 2: " ++ show (slopeTrees rows (1,1) *
                                slopeTrees rows (3,1) *
                                slopeTrees rows (5,1) *
                                slopeTrees rows (7,1) *
                                slopeTrees rows (1,2)))

-- A line of the input is represented by a list of Booleans
--  True if there is a tree, False otherwise
readL :: String -> [Bool]
readL [] = []
readL ('#':s) = True : readL s
readL ('.':s) = False : readL s
readL _ = error "unexpected character"

-- The area is a matrix of booleans
--  Counting the number of trees along a slope from the top-left corner
slopeTrees :: [[Bool]] -> (Int,Int) -> Int
slopeTrees rs (dx,dy) =
  treesFrom rs (length (head rs),length rs) (0,0) (dx,dy) 0

-- rs representation of the area, (n,m) dimentions of the area,
-- (x,y) present position, (dx,dy) slope, trs number of trees so far 
treesFrom :: [[Bool]] -> (Int,Int) -> (Int,Int) -> (Int,Int) -> Int -> Int
treesFrom rs (n,m) (x,y) (dx,dy) trs =
  let y' = y + dy
      x' = if x+dx < n then x+dx else x+dx-n
  in if y' >= m
       then trs
       else treesFrom rs (n,m) (x',y') (dx,dy)
                      (trs + if rs!!y'!!x' then 1 else 0)




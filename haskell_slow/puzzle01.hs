module Main where

import System.Environment
import Data.List

main :: IO ()
main = do
  args <- getArgs
  mainArg (head args)

mainArg :: String -> IO ()
mainArg fileName = do
  input <- readFile fileName
  let nums = inputList input
      result1 = puzzle1 nums
      result2 = puzzle2 nums
  putStrLn "Anser to Part 1:"
  putStrLn (show result1)
  putStrLn "Answer to Part 2:"
  putStrLn (show result2)

inputList :: String -> [Int]
inputList = map read . words

puzzle1 :: [Int] -> Int
puzzle1 l = let l' = sort l
                (x,y) = search2 l' 2020
            in x * y

-- Search two elements in a list that sum to a give value
search2 :: [Int] -> Int -> (Int,Int)
search2 l sum = let l' = sort l
                in searchSum l' (reverse l') sum
            
-- Searching in two list, first increasing, second decreasing
searchSum :: [Int] -> [Int] -> Int -> (Int,Int)
searchSum xs@(x:xs') ys@(y:ys') z
  | x+y > z = searchSum xs ys' z
  | x+y < z = searchSum xs' ys z
  | otherwise  = (x,y)
searchSum _ _ _ = (-1,-1)

puzzle2 :: [Int] -> Int
puzzle2 l = let l' = sort l
                (x,y,z) = search3 l' 2020
            in x * y * z

-- Search three elements in a list that sum to a give value
search3 :: [Int] -> Int -> (Int,Int,Int)
search3 xs = search3ord (sort xs)

-- Assuming the list is ordered
search3ord :: [Int] -> Int -> (Int,Int,Int)
search3ord (x:xs) sum =
  let (y,z) = searchSum xs (reverse xs) (sum - x)
  in if (y,z) == (-1,-1) then search3 xs sum else (x,y,z)
search3ord _ _ = (-1,-1,-1)

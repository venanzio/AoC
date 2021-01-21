{- Day 1
   The solution is essentially the same as the "quick" one
   I avoided sorting the list many times in part 2
   by sorting it just once at the beginning

   CHALLENGE:
   Generalize it to:
     find k elements out of a list that add up to a given value
     (Part 1 has k=2, Part 2 has k=3)
-}

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
      result1 = puzzle1 nums  -- part1 nums
      result2 = puzzle2 nums  -- part2 nums
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
                in search2Sum l' (reverse l') sum
            
-- Searching in two list, first increasing, second decreasing
search2Sum :: [Int] -> [Int] -> Int -> (Int,Int)
search2Sum xs@(x:xs') ys@(y:ys') z
  | x+y > z = search2Sum xs ys' z
  | x+y < z = search2Sum xs' ys z
  | otherwise  = (x,y)
search2Sum _ _ _ = (-1,-1)

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
  let (y,z) = search2Sum xs (reverse xs) (sum - x)
  in if (y,z) == (-1,-1) then search3 xs sum else (x,y,z)
search3ord _ _ = (-1,-1,-1)

{- GENERALIZATION

Find k elements that add up to the given value

-}

searchSum :: Int -> [Int] -> Int -> Maybe [Int]
searchSum 0 _ s = if s==0 then Just [] else Nothing
searchSum k (x:xs) s = case searchSum (k-1) xs (s-x) of
  Just ys -> Just (x:ys)
  Nothing -> searchSum k xs s
searchSum _ _ _ = Nothing

part1 xs = case searchSum 2 xs 2020 of
  Just [x1,x2] -> (x1,x2,x1*x2)
  _ -> error "No solution to Part 1"

part2 xs = case searchSum 3 xs 2020 of
  Just [x1,x2,x3] -> (x1,x2,x3,x1*x2*x3)
  _ -> error "No solution to Part 2"


{- SUBSET-SUM PROBLEM
   Finding any subset with the given sum
   (we assume all entries are positive)
   NP-complete
-}

subSum :: [Int] -> Int -> Maybe [Int]
subSum xs = subsetSum (reverse $ sort xs)

subsetSum :: [Int] -> Int -> Maybe [Int]
subsetSum _ 0 = Just []
subsetSum [] s = Nothing
subsetSum (x:xs) s
  | s < 0  = Nothing
  | otherwise = case subsetSum xs (s-x) of
      Just ys -> Just (x:ys)
      Nothing -> subsetSum xs s

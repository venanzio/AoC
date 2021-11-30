{- Day 1
   The solution is essentially the same as the "quick" one
   I avoided sorting the list many times in part 2
   by sorting it just once at the beginning
-}

module Main where

import System.Environment
import Data.List

import RandomList

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
   Find k elements of a list that add up to the given value
     (Part 1 has k=2, Part 2 has k=3)
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

-- We sort the list in decreasing order before searching
subSum :: [Int] -> Int -> Maybe [Int]
subSum xs s = subSumBound (reverse $ sort xs) s (sum xs)

-- Keeping track of the sum of the remaining list (upper bound of subset-sum)
subSumBound :: [Int] -> Int -> Int -> Maybe [Int]
subSumBound _  0 _ = Just []
subSumBound [] s _ = Nothing
subSumBound _  s b | b<s = Nothing  -- sum not achievable because higher than bound
subSumBound (x:xs) s b
  | s < 0  = Nothing
  | otherwise = case subSumBound xs (s-x) (b-x) of
      Just ys -> Just (x:ys)
      Nothing -> subSumBound xs s (b-x)

-- Looking for counterexamples
-- List of counterexample (excluding trivially low ones)

counter :: [Int] -> [Int]
counter l = [x | x <- [(minimum l)..(sum l)], subSum l x == Nothing]



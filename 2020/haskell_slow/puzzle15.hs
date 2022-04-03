-- Advent of Code 2020, day 15

{- This is based on Van Eck's sequence
   OEIS: A181391 - https://oeis.org/A181391
   Nuberphile: https://youtu.be/etMJxB-igrc
-}

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

-- input argument given as Haskell list, for example: "[15,12,0,14,3,1]"
main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)
  
xs = [15,12,0,14,3,1] :: [Int]

puzzle :: String -> IO ()
puzzle input = do
  let xs = (read input)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- data type and main algorithm

type NumTurns = (M.Map Int Int, Int, Int)
-- map numbers to turn of last occurrence, new number, current turn

-- initial numbers from a given list
initNums :: [Int] -> NumTurns
initNums xs = let n = length xs
                  xs' = take (n-1) xs
                  x = last xs
              in (M.fromList (zip xs' [1..]), x, n)

-- play one turn: insert the new element and determine the next
playTurn :: NumTurns -> NumTurns
playTurn (nums, x, turn) = case M.lookup x nums of
  Nothing -> (M.insert x turn nums, 0, turn+1)
  Just t  -> (M.insert x turn nums, turn-t, turn+1)

-- play up to a given turn
playTo :: NumTurns -> Int -> Int
playTo nl@(_,x,t) end = if t==end then x else playTo (playTurn nl) end

-- play from the beginning to a given turn
play :: [Int] -> Int -> Int
play xs end = playTo (initNums xs) end

-- Part 1

part1 :: [Int] -> Int
part1 xs = play xs 2020

-- Part 2

part2 :: [Int] -> Int
part2 xs = play xs 30000000


{-
Note that the apparently equivalent version of playTurn:

playTurn :: NumTurns -> NumTurns
playTurn (nums, x, turn) = (M.insert x turn nums, x', turn+1)
  where x' = case M.lookup x nums of
               Nothing -> 0
               Just t -> turn-t
               
is inefficient, because it doesn't force the evaluation of the case expression.
-}

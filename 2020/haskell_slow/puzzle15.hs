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
  putStrLn ("input: " ++ show xs)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- data type and main algorithm

type NumTurns = M.Map Int Int
-- maps each number to the turn of its last occurrence

init :: [Int] -> NumTurns
init xs = M.fromList (zip xs [1..])

-- playing the memory game with a starting number x at given turn
playSeq :: NumTurns -> Int -> Int -> [Int]
playSeq nums x turn = x : playSeq nums' x' (turn+1)
  where nums' = M.insert x turn nums
        x' = case M.lookup x nums of
               Nothing -> 0
               Just t -> turn-t

-- Playing with a list of starting numbers
playStart :: [Int] -> [Int]
playStart xs = playS M.empty xs 1
  where playS nums [x] turn = playSeq nums x turn
        playS nums (x:xs) turn = x : playS (M.insert x turn nums) xs (turn+1)
        playS nums [] turn = playSeq nums 0 turn -- if the list is empty, we start with 0

-- number spoken at a given turn
numTurn :: [Int] -> Int -> Int
numTurn xs turn = (playStart xs)!!(turn-1) -- because indices start at 0

-- Part 1

part1 :: [Int] -> Int
part1 xs = numTurn xs 2020

-- Part 2

part2 :: [Int] -> Int
part2 xs = numTurn xs 30000000

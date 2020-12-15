module PuzzleInput where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

input :: [Int]
input = [15,12,0,14,3,1]

type NumList = (M.Map Int Int, Int, Int)

-- Part 1

start :: [Int] -> NumList
start input = (M.fromList (zip input [1..]), length input, last input)

next :: NumList -> NumList
next (nlist, turn, new) =
  case M.lookup new nlist of
    Nothing -> (M.insert new turn nlist, turn+1, 0)
    Just t -> (M.insert new turn nlist, turn+1, turn-t)
    
play :: NumList -> Int -> Int
play nl@(_,t,n) end = if t==end then n else play (next nl) end

result :: Int
result = play (start input) 2020

-- Part 2

result2 :: Int
result2 = play (start input) 30000000

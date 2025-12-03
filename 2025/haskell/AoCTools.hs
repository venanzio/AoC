-- Advent of Code: Useful Functions
--  Venanzio Capretta 2020 - 2025

module AoCTools where

import Data.List
import Data.Function (on)
import qualified Data.Map as M

-- NUMBERS

-- number of decimal digits of a natural
dig10 :: Int -> Int
dig10 n = if n<10 then 1 else 1 + dig10 (n `div` 10)

divisible :: Int -> Int -> Bool
divisible x y = rem x y == 0

divisors :: Int -> [Int]
divisors x = [y | y <- [1 .. x `div` 2], divisible x y]

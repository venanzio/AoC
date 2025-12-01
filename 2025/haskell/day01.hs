-- Advent of Code 2025, day 1
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Data.Char
import Control.Applicative
import qualified Data.Map as M

import FunParser_old
import AoCTools_old

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))
  -- putStrLn (show (rot2 xs))

-- Parsing the input

pData :: Parser Int
pData = (char 'R' >> natural) <|>
        (char 'L' >> natural >>= return . negate)

pInput :: Parser [Int]
pInput = pLines pData

-- Part 1

rotate :: [Int] -> Int
rotate = rotateAux 50 where
  rotateAux x [] = 0
  rotateAux x (y:ys) = let x' = (x+y) `mod` 100
                       in if x' == 0 then 1 + rotateAux x' ys else rotateAux x' ys


part1 :: [Int] -> Int
part1 = rotate

-- Part 2

rotate2 :: [Int] -> Int
rotate2 = rotateAux 50 where
  rotateAux x [] = 0
  rotateAux x (y:ys) = let xRaw = x+y
                           inc = if y > 0 then xRaw `div` 100
                                 else (((100-x) `mod` 100) - y) `div` 100
                           x' = xRaw `mod` 100
                       in inc + rotateAux x' ys

rot2 :: [Int] -> [(Int,Int)]
rot2 = rotateAux 50 where
  rotateAux x [] = []
  rotateAux x (y:ys) = let xRaw = x+y
                           inc = if y > 0 then xRaw `div` 100
                                 else ((100-x) - y) `div` 100
                           x' = xRaw `mod` 100
                       in (inc,x') : rotateAux x' ys

part2 :: [Int] -> Int
part2 = rotate2


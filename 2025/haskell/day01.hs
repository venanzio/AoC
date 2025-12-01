-- Advent of Code 2025, day 1
--  Venanzio Capretta

module Main where

import System.Environment
import Control.Applicative

import FunParser

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

-- Parsing the input

pData :: Parser Int
pData = (char 'R' >> natural) <|>
        (char 'L' >> natural >>= return . negate)

pInput :: Parser [Int]
pInput = pLines pData

-- Part 1

rot1 :: Int -> Int -> (Int,Int)
rot1 x y = let x' = (x+y) `mod` 100
               inc = if x' == 0 then 1 else 0
           in (inc, x')

rotate :: (Int -> Int -> (Int,Int) ) -> [Int] -> Int
rotate rot = rotateAux 50 where
  rotateAux x [] = 0
  rotateAux x (y:ys) = let (inc,x') = rot x y
                       in inc + rotateAux x' ys

part1 :: [Int] -> Int
part1 = rotate rot1

-- Part 2

rot2 :: Int -> Int -> (Int,Int)
rot2 x y = (inc, x' `mod` 100) where
  x' = x+y
  x'' = if x==0 then -y else 100-x-y
  inc = if y>=0 then x' `div` 100 else x'' `div` 100

part2 :: [Int] -> Int
part2 = rotate rot2


-- Advent of Code 2024, day 1
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List

import FunParser

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
      as = sort (fst xs)
      bs = sort (snd xs)
  putStrLn ("Part 1: " ++ show (part1 as bs))
  putStrLn ("Part 2: " ++ show (part2 as bs))

-- Parsing the input

pData :: Parser (Int,Int)
pData = do a <- natural
           b <- natural
           return (a,b)

pInput :: Parser ([Int],[Int])
pInput = do pairs <- pLines pData
            return (unzip pairs)

-- Part 1

part1 :: [Int] -> [Int] -> Int
part1 = (sum .) . (zipWith (\a b -> abs (a-b)))
      

-- Part 2

splitEq :: [Int] -> (Int,Int,[Int])
splitEq [] = (0,0,[])
splitEq (x:xs) = let (xs1,xs2) = span (==x) xs
                 in (x,length xs1+1,xs2)
                    
similarity :: (Int,Int,[Int]) -> (Int,Int,[Int]) -> Int
similarity (a,na,as2) (b,nb,bs2)
  | na == 0 || nb == 0 = 0
  | a < b = similarity (splitEq as2) (b,nb,bs2)
  | a > b = similarity (a,na,as2) (splitEq bs2)
  | otherwise = a*na*nb + similarity (splitEq as2) (splitEq bs2)

part2 :: [Int] -> [Int] -> Int
part2 as bs = similarity (splitEq as) (splitEq bs)

-- Advent of Code 2021, day 9

-- IDEA: use Dijkstra's algorithm for part 2

module Main where

import System.Environment
import Data.List
import Data.Char

import Control.Applicative
import qualified Data.Map as M

import FunParser
import AoCTools

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

pData :: Parser [Int]
pData = some (digit >>= \d -> return (read [d]))

pInput :: Parser [[Int]]
pInput = pLines pData

-- Part 1

near :: (Int,Int) -> [(Int,Int)]
near (i,j) = [(i-1,j),(i+1,j),(i,j-1),(i,j+1)]

adjacent :: [[Int]] -> (Int,Int) -> [Int]
adjacent xs (i,j) = [xs!!i'!!j' |
                     (i',j') <- near (i,j),
                     i'>=0, j'>=0, i' < length xs, j' < length (head xs)]

lowPoint ::  Int -> [Int] -> Bool
lowPoint x ys = all (x<) ys

lowPoints :: [[Int]] -> [Int]
lowPoints xs = [xs!!i!!j | i <- [0..length xs -1], j<- [0..length (head xs) -1],
                           lowPoint (xs!!i!!j) (adjacent xs (i,j))]

part1 :: [[Int]] -> Int
part1 xs = sum $ map (+1) $ lowPoints xs

-- Part 2

adj :: [[Int]] -> (Int,Int) -> [(Int,Int)]
adj xs (i,j) = [(i',j') |
                (i',j') <- near (i,j),
                i'>=0, j'>=0, i' < length xs, j' < length (head xs)]

basin :: [[Int]] -> (Int,Int) -> [(Int,Int)]
basin xs (i,j) = basCum [] [(i,j)]
  where basCum as [] = as
        basCum as bs =
          let as' = as++bs
              cs = nub [(i',j') | (i,j) <- bs, (i',j') <- adj xs (i,j),
                        xs!!i'!!j' >= xs!!i!!j, xs!!i'!!j' < 9] \\ as'
          in basCum as' cs

lPoints :: [[Int]] -> [(Int,Int)]
lPoints xs = [(i,j) | i <- [0..length xs -1], j<- [0..length (head xs) -1],
                      lowPoint (xs!!i!!j) (adjacent xs (i,j))]



basins :: [[Int]] -> [[(Int,Int)]]
basins xs = map (basin xs) (lPoints xs)

part2 :: [[Int]] -> Int
part2 xs = product $ take 3 $ reverse $ sort $ map length (basins xs)

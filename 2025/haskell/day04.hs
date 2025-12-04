-- Advent of Code 2025, day 4
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
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
      grid = rollCount xs (initRolls xs)
  putStrLn ("Part 1: " ++ show (part1 grid))
  putStrLn ("Part 2: " ++ show (part2 grid))

-- Parsing the input

pData :: Parser [Int]
pData = do line <- label
           return (filter (\k -> line!!k == '@') [0 .. length line -1])

pInput :: Parser [Point]
pInput = do rows <- pLines pData
            return [(x,y) | x <- [0 .. length rows -1], y <- rows!!x]
-- Part 1

initRolls :: [Point] -> Map2D Int
initRolls = M.fromList . map (\p -> (p,0))

rollCount :: [Point] -> Map2D Int -> Map2D Int
rollCount [] grid = grid
rollCount (p:ps) grid = rollCount ps (foldr (M.adjust (+1))  grid (neighbours p))

accessible :: Map2D Int -> [Point]
accessible = M.keys . M.filter (<4)

part1 :: Map2D Int -> Int
part1 grid = length $ accessible grid

-- Part 2

rollDec :: [Point] -> Map2D Int -> Map2D Int
rollDec [] grid = grid
rollDec (p:ps) grid = rollDec ps (foldr (M.adjust (\x -> x-1))  grid (neighbours p))

rollOff :: [Point] -> Map2D Int -> Map2D Int
rollOff ps grid = rollDec ps (foldr M.delete grid ps)

remove :: Map2D Int -> Int
remove grid = if as == [] then 0 else length as + remove grid' where
  as = accessible grid
  grid' = rollOff as grid

part2 :: Map2D Int -> Int
part2 = remove

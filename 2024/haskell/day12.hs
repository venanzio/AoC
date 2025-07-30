-- Advent of Code 2024, day 12
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
-- import Data.Char
-- import Control.Applicative
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
  let m = stringsMap (lines input)
  putStrLn (show (fst $ region m))
  putStrLn ("Part 1: " ++ show (part1 m))
  putStrLn ("Part 2: " ++ show (part2 m))

-- Part 1

-- Separate a region from the rest of the map
region :: Map2D Char -> ([Point], Map2D Char)
region m
  | M.null m = ([],m)
  | otherwise  = let (p,kind) = M.findMin m in
      regionAux [p] kind m

regionAux :: [Point] -> Char -> Map2D Char -> ([Point], Map2D Char)
regionAux [] kind m = ([],m)
regionAux (p:ps) kind m
  | M.lookup p m == Just kind =
       mapFst (p :) $ regionAux (neighboursHV p ++ ps) kind (M.delete p m)
  | otherwise = regionAux ps kind m

part1 :: Map2D Char -> Int
part1 _ = 1

-- Part 2

part2 :: Map2D Char -> Int
part2 _ = 2

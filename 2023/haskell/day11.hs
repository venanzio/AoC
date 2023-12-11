-- Advent of Code 2023, day 0

--  Venanzio Capretta

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
  input <- readFile fileName >>= return . lines
  let image = expand input
  putStrLn (showI image)
  putStrLn ("Part 1: " ++ show (part1 image))
  putStrLn ("Part 2: " ++ show (part2 image))

-- Parsing the input

type Image = [String]

showI [] = ""
showI (r:rs) = r ++ '\n':showI rs

expandR :: Image -> Image
expandR [] = []
expandR (r:rs) = if all (=='.') r then r:r:expandR rs else r:expandR rs

expand :: Image -> Image
expand = transpose . expandR . transpose . expandR


-- Part 1

part1 :: Image -> Int
part1 _ = 1

-- Part 2

part2 :: Image -> Int
part2 _ = 2

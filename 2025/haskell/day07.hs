-- Advent of Code 2025, day 7
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

-- Parsing the input

pData :: Parser [String]
pData = some line

pInput :: Parser (Int, Int, Map2D Char)
pInput = do ls <- some line
            let width = length (ls!!1)
                height = length ls
                manifold = mMapF (\c -> if c=='.' then Nothing else Just c) ls
            return (width, height, manifold)

-- Part 1

part1 :: (Int, Int, Map2D Char) -> Int
part1 _ = 1

-- Part 2

part2 :: (Int, Int, Map2D Char) -> Int
part2 _ = 2

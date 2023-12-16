-- Advent of Code 2023, day 16
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
  input <- readFile fileName >>= return.lines
  let width = length (input!!0)
      height = length input
      contraption = pContraption input
  putStrLn ("Part 1: " ++ show (part1 width height contraption))
  putStrLn ("Part 2: " ++ show (part2 width height contraption))

-- Parsing the input

type Point = (Int,Int)
type Contraption = M.Map Point Char

pContraption :: [String] -> Contraption
pContraption = mMapF (\c -> if c `elem` "/\\-|" then Just c else Nothing)

-- Part 1

data Direction - Up | Down | Left | Right
type Energized = M.Map Point [Direction]

beam :: Point -> Direction -> Energized
beam p d = undefined

part1 :: Int -> Int -> Contraption -> Int
part1 width height contraption = 1

-- Part 2

part2 :: Int -> Int -> Contraption -> Int
part2 width height contraption = 2

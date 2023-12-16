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

pData :: Parser ()
pData = return ()

pInput :: Parser [()]
pInput = pLines pData

pContraption :: [String] -> Contraption
pContraption = undefined

-- Part 1

part1 :: Int -> Int -> Contraption -> Int
part1 width height contraption = 1

-- Part 2

part2 :: Int -> Int -> Contraption -> Int
part2 width height contraption = 2

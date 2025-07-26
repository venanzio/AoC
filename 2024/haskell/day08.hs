-- Advent of Code 2024, day 8
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
-- import Data.Char
-- import Control.Applicative
-- import qualified Data.Map as M

import FunParser
import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let lns = lines input
      maxX = length (lns!!0) - 1
      maxY = length lns - 1
      antennas = stringsMap (lines input)
  putStrLn (show maxX ++ " - " ++ show maxY)
  putStrLn (show antennas)
  putStrLn ("Part 1: " ++ show (part1 maxX maxY antennas))
  putStrLn ("Part 2: " ++ show (part2 maxX maxY antennas))

-- Parsing the input

pData :: Parser ()
pData = return ()

pInput :: Parser [()]
pInput = pLines pData

-- Part 1

part1 :: Int -> Int -> Map2D Char -> Int
part1 maxX maxY antennas = 1

-- Part 2

part2 :: Int -> Int -> Map2D Char -> Int
part2 maxX maxY antennas = 2

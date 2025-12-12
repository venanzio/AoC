-- Advent of Code 2025, day 12
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
  input <- readFile fileName
  let xs = parseAll pInput input
   putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pShape :: Parser [Point]
pShape = do natural >> symbol ":"
            sh <- some neLine
            return [(x,y) | y <- [0..length sh -1], x <- [0..length (sh!!y) -1],
                            (sh!!y)!!x == '#']

pRegion :: Parser (Int,Int,[Int])
pRegion = do width <- natural
             symbol "x"
             height <- natural
             symbol ":"
             shapes <- pLine (some natural)
             return (width,height,shapes)


pInput :: Parser ([[Point]],[(Int,Int,[Int])])
pInput = do shapes <- some pShape
            regions <- some pRegion
            return (shapes,regions)

-- Part 1

part1 :: ([[Point]],[(Int,Int,[Int])]) -> Int
part1 _ = 1

-- Part 2

part2 :: ([[Point]],[(Int,Int,[Int])]) -> Int
part2 _ = 2

-- Advent of Code 2020, day 5

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let grps = parseAll groups input
      p1counts = map (length.anyAnswer) grps
      p2counts = map (length.allAnswer) grps
  putStrLn ("Part 1: " ++ show (sum p1counts)) 
  putStrLn ("Part 2: " ++ show (sum p2counts)) 
        
-- Parsing

groups :: Parser [[String]]
groups = blocks (some line)

-- Part 1
-- questions to which any group member answered "yes"

anyAnswer :: [String] -> String
anyAnswer = nub . foldl union ""

-- Part 2
-- question to which all group members answered "yes"

allAnswer :: [String] -> String
allAnswer = nub . foldl intersect ['a'..'z']

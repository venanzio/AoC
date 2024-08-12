-- Advent of Code 2017, day 5

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
  putStrLn "Advent of Code 2017, day 5"
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pInput :: Parser [Int]
pInput = some integer

-- Part 1

type Maze = M.Map Int Int

jump_around :: (Int->Int) -> Maze -> Int
jump_around step m = ja_from m 0 0 where
  ja_from m i s =  case M.lookup i m of
    Nothing -> s
    Just j -> ja_from (M.adjust step i m) (i+j) (s+1)

part1 :: [Int] -> Int
part1 js = jump_around (+1) (M.fromList $ zip [0..] js)

-- Part 2

part2 :: [Int] -> Int
part2 js  = jump_around (\j -> if j>=3 then j-1 else j+1)
                        (M.fromList $ zip [0..] js)


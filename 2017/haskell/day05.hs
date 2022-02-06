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
  putStrLn(show xs)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input


pInput :: Parser [Int]
pInput = some integer

-- Part 1

-- two ways lists
type TwoList a = ([a],[a]) -- front and back in reverse order

tlInit :: [a] -> TwoList a
tlInit l = (l,[])

move :: Int -> TwoList a -> Maybe (TwoList a)
move 0 l = Just l
move n (x:xs,ys) | n>0 = move (n-1) (xs,x:ys)
move n (xs,y:ys) | n<0 = move (n+1) (y:xs,ys)
move _ _ = Nothing



part1 :: [Int] -> Int
part1 _ = 1

-- Part 2

part2 :: [Int] -> Int
part2 _ = 2

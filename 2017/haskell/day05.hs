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

exMaze = mInit [0,3,0,1,-3]

-- Parsing the input


pInput :: Parser [Int]
pInput = some integer

-- Part 1

-- two ways lists
type Maze = ([Int],[Int]) -- front and back in reverse order

mInit :: [Int] -> Maze
mInit l = (l,[])

mSound :: Maze -> Bool
mSound (front,_) = not (null front)

mMove :: Int -> Maze -> Maybe (Maze)
mMove 0 m | mSound m = Just m
mMove n (x:xs,ys) | n>0 = mMove (n-1) (xs,x:ys)
mMove n (xs,y:ys) | n<0 = mMove (n+1) (y:xs,ys)
mMove _ _ = Nothing

mRead :: Maze -> Maybe Int
mRead (x:_,_) = Just x
mRead _ = Nothing

mWrite :: Int -> Maze -> Maybe Maze
mWrite x (_:xs,ys) = Just (x:xs,ys)
mWrite _ _ = Nothing

mStep :: Maze -> Maybe Maze
mStep m = do x <- mRead m
             m' <- mWrite (x+1) m
             mMove x m'

mTrip :: Maze -> Int
mTrip m = case mStep m of
  Nothing -> 1
  Just m' -> 1 + mTrip m'

part1 :: [Int] -> Int
part1 = mTrip . mInit

-- Part 2

part2 :: [Int] -> Int
part2 _ = 2

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
mSound = not . null

mMove :: Int -> Maze -> Maybe (Maze)
mMove 0 m | mSound m = Just m
mMove n (x:xs,ys) | n>0 = mMove (n-1) (xs,x:ys)
mMove n (xs,y:ys) | n<0 = mMove (n+1) (y:xs,ys)
mMove _ _ = Nothing

mRead :: Maze -> Int
mRead (x:_,_) = x
mRead _ = error "out of bounds"

mWrite :: Int -> Maze -> Maze
mWrite x (_:xs,ys) = (x:xs,ys)
mWrite _ _ = error "out of bounds"

mStep :: Maze -> Maybe Maze
mStep m = let x = mRead m
          in case mMove x m of
               Nothing -> Nothing
               Just m' -> Just (mWrite (x+1) m')

mTrip :: Maze -> Int
mTrip m =
  let x = mRead m
      m1 = mWrite (x+1) m
  in case mMove x m1 of
       Just m2 -> (mTrip m2) + 1
       Nothing -> 0

part1 :: [Int] -> Int
part1 = mTrip . mInit

-- Part 2

part2 :: [Int] -> Int
part2 _ = 2

-- Advent of Code 2022, day 5
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
  let (sts,mvs) = parseAll pInput input
  putStrLn ("Part 1: " ++ part1 sts mvs)
  putStrLn ("Part 2: " ++ part2 sts mvs)

-- Parsing the input

type Stack = [Char]
type Stacks = M.Map Int Stack
type Move = (Int,Int,Int)

inputStacks :: String -> [String]
inputStacks s = let (s1,s2) = break (=='1') s
                in init (lines s1)

compressLine :: String -> String
compressLine s | length s <3 = []
               | otherwise   = (s!!1) : compressLine (drop 4 s)


stackMatrix :: [String] -> [Stack]
stackMatrix = map (dropWhile (==' ')) . transpose . map compressLine

stackMap :: String -> Stacks
stackMap = listMap 1 . stackMatrix . inputStacks

pMove :: Parser Move
pMove = do symbol "move"
           n <- natural
           symbol "from"
           fr <- natural
           symbol "to"
           to <- natural
           return (n,fr,to)

pInput :: Parser (Stacks,[Move])
pInput = do sts <- chunk
            line
            moves <- some pMove
            return (stackMap sts,moves)

-- Part 1

move :: Stacks -> Move -> Stacks
move st (k,n,m) =
  let xs = take k (st M.! n)
  in M.adjust (drop k) n (M.adjust (reverse xs ++) m st)

moves :: Stacks -> [Move] -> Stacks
moves = foldl move

stackTops :: Stacks -> String
stackTops = map head . M.elems

part1 :: Stacks -> [Move] -> String
part1 sts mvs = stackTops $ moves sts mvs

-- Part 2

move2 :: Stacks -> Move -> Stacks
move2 st (k,n,m) =
  let xs = take k (st M.! n)
  in M.adjust (drop k) n (M.adjust (xs ++) m st)

moves2 :: Stacks -> [Move] -> Stacks
moves2 = foldl move2

part2 :: Stacks -> [Move] -> String
part2 sts mvs = stackTops $ moves2 sts mvs


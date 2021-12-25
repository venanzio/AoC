-- Advent of Code 2021, day 25

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
  putStrLn (show $ xs M.! (8,17))
  -- putStrLn ("Part 1: " ++ show (part1 xs))
  -- putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type Floor = M.Map (Int,Int) Char

pRow :: Parser [Char]
pRow =  many (char '.' <|> char '>' <|> char 'v')

pInput :: Parser Floor
pInput = pLines pRow >>= return . mMap 

-- Part 1

nextR :: Floor -> (Int,Int) -> (Int,Int)
nextR floor (x,y) = case M.lookup (x,y) floor of
  Just '>' -> case M.lookup (x+1,y) of
           Just ..

part1 :: Floor -> Int
part1 _ = 1

-- Part 2

part2 :: Floor -> Int
part2 _ = 2

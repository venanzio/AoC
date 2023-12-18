-- Advent of Code 2023, day 18
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
      t = trench (map (\(d,m,_)->(d,m)) xs)
      a = area t
  --putStrLn (show (head t) ++ show (last t))
  -- putStrLn (showTrench t [])
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type RGB = String

readRGB :: String -> RGB
readRGB = id

pData :: Parser (Char,Int,RGB)
pData = do direction <- item
           meters <- natural
           symbol "(#"
           rgb <- word >>= return.readRGB
           symbol ")"
           return (direction,meters,rgb)

pInput :: Parser [(Char,Int,RGB)]
pInput = pLines pData

-- Part 1

showTrench :: [Point] -> [Point] -> String
showTrench t a = unlines
  [[showPoint (x,y)
   | x <- [minimum (map fst t)..2 {-maximum (map fst t)-}]
   ]
  | y <- [minimum (map snd t)..maximum (map snd t)]
  ]
  where showPoint p =
          if p `elem` t
          then if p `elem` a then '*' else '#'
          else if p `elem` a then '+' else '.'

trench :: [(Char,Int)] -> [Point]
trench = trenchFrom (0,0) where
  trenchFrom (x,y) [] = []
  trenchFrom (x,y) (('U',n):cis) = [(x,y-i) | i <- [0..n-1]] ++ trenchFrom (x,y-n) cis
  trenchFrom (x,y) (('D',n):cis) = [(x,y+i) | i <- [0..n-1]] ++ trenchFrom (x,y+n) cis
  trenchFrom (x,y) (('L',n):cis) = [(x-i,y) | i <- [0..n-1]] ++ trenchFrom (x-n,y) cis
  trenchFrom (x,y) (('R',n):cis) = [(x+i,y) | i <- [0..n-1]] ++ trenchFrom (x+n,y) cis

area :: [Point] -> [Point]
area ps = ps ++
  enclosed [(x,y)
           | x <- [minimum (map fst ps)..maximum (map fst ps)]
           , y <- [minimum (map snd ps)..maximum (map snd ps)]
           ]
           ps

part1 ::  [(Char,Int,RGB)] -> Int
part1 = length . nub . area . trench . map (\(d,m,_)->(d,m))

-- Part 2

part2 ::  [(Char,Int,RGB)] -> Int
part2 _ = 2

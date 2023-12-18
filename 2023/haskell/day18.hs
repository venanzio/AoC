-- Advent of Code 2023, day 18
--  Venanzio Capretta

module Main where

import System.Environment
import Numeric (readHex)
import FunParser
import AoCTools (Point,polyArea)

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
      t1 = trench (map fst xs)
      t2 = trench (map snd xs)
  putStrLn ("Part 1: " ++ show (polyArea t1))
  putStrLn ("Part 2: " ++ show (polyArea t2))

-- Parsing the input

type DirDist = (Char,Int)

hex :: String -> Int
hex s = case readHex s of
  [(x,"")] -> x
  _ -> error "not an hex"
  

readRGB :: String -> DirDist
readRGB s =
  ( case (hex [last s]) of
      0 -> 'R'
      1 -> 'D'
      2 -> 'L'
      3 -> 'U'
      _ -> error "invalid direction"
  , hex (take 5 s)
  )

pData :: Parser (DirDist,DirDist)
pData = do direction <- item
           meters <- natural
           symbol "(#"
           rgb <- word >>= return.readRGB
           symbol ")"
           return ((direction,meters),rgb)

pInput :: Parser [(DirDist,DirDist)]
pInput = pLines pData

-- Part 1 and 2

trench :: [DirDist] -> [Point]
trench = trenchFrom (0,0)

trenchFrom :: Point -> [DirDist] -> [Point]
trenchFrom p [] = [p]
trenchFrom (x,y) (('U',n):cis) = (x,y) : trenchFrom (x,y-n) cis
trenchFrom (x,y) (('D',n):cis) = (x,y) : trenchFrom (x,y+n) cis
trenchFrom (x,y) (('L',n):cis) = (x,y) : trenchFrom (x-n,y) cis
trenchFrom (x,y) (('R',n):cis) = (x,y) : trenchFrom (x+n,y) cis


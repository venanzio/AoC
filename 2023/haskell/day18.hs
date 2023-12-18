-- Advent of Code 2023, day 18
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Data.Char
import Control.Applicative
import qualified Data.Map as M

import Numeric (readHex)

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
      t2 = rgbTrench (map (\(_,_,rgb) -> rgb) xs)
      a = area t
  putStrLn (show t2)
  -- putStrLn (showTrench t [])
  --putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 t2))

-- Parsing the input

type RGB = (Char,Int)

hex :: String -> Int
hex s = case readHex s of
  [(x,"")] -> x
  _ -> error "not an hex"
  

readRGB :: String -> RGB
readRGB s =
  ( case (hex [last s]) of
      0 -> 'R'
      1 -> 'D'
      2 -> 'L'
      3 -> 'U'
      _ -> error "invalid direction"
  , hex (take 5 s)
  )

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

rgbTrench :: [RGB] -> [Point]
rgbTrench = rgbt (0,0) where
  rgbt p [] = [p]
  rgbt (x,y) (('U',n):cis) = (x,y) : rgbt (x,y-n) cis
  rgbt (x,y) (('D',n):cis) = (x,y) : rgbt (x,y+n) cis
  rgbt (x,y) (('L',n):cis) = (x,y) : rgbt (x-n,y) cis
  rgbt (x,y) (('R',n):cis) = (x,y) : rgbt (x+n,y) cis

trenchLength :: [Point] -> Int
trenchLength [] = 0
trenchLength [p] = 0
trenchLength ((x0,y0):(x1,y1):ps) = trenchLength ((x1,y1):ps) +
  if x0==x1 then abs (y1-y0) else abs (x1-x0)

shoelace :: [Point] -> Int
shoelace ps = abs (sum [rectangle p0 p1 | (p0,p1) <- zip ps (tail ps)])
              + trenchLength ps `div` 2 +1



rectangle :: Point -> Point -> Int
rectangle (x0,y0) (x1,y1) =
  if y0==y1 then (x1-x0)*y0
            else 0

part2 ::  [Point] -> Int
part2 = shoelace 

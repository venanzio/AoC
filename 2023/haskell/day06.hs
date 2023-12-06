-- Advent of Code 2023, day 6
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
  putStrLn ("Part 1: " ++ show (part1 (input1 xs)))
  putStrLn ("Part 2: " ++ show (part2 (input2 xs)))

-- Parsing the input

pInput :: Parser ([String],[String])
pInput = do symbol "Time:"
            ts <- pLine (many word)
            symbol "Distance:"
            ds <- pLine (many word)
            return (ts,ds)

input1 :: ([String],[String]) -> [(Int,Int)]
input1 (ts,ds) = zip (map read ts) (map read ds)

input2 :: ([String],[String]) -> (Int,Int)
input2 (ts,ds) = (read (concat ts), read (concat ds))

-- Part 1

nextUp :: Double -> Int
nextUp x = let x1 = floor x
               x2 = ceiling x
           in if x1==x2 then x1+1 else x2

nextDown :: Double -> Int
nextDown x = let x1 = floor x
                 x2 = ceiling x
             in if x1==x2 then x1-1 else x1
               
race :: (Int,Int) -> Int
race (time,distance) =
  let t = fromIntegral time
      d = fromIntegral distance
      disc = sqrt (t^2-4*d)
      x1 = nextUp ((t - disc)/2)
      x2 = nextDown ((t + disc)/2)
  in x2 - x1 + 1

part1 :: [(Int,Int)] -> Int
part1 = product . map race 

-- Part 2

part2 :: (Int,Int) -> Int
part2 = race

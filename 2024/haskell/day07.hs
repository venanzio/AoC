-- Advent of Code 2024, day 7
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
-- import Data.Char
import Control.Applicative
-- import qualified Data.Map as M

import FunParser
-- import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn (show xs)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser (Int,[Int])
pData = do s <- natural
           symbol ":"
           xs <- some natural
           return (s,xs)

pInput :: Parser [(Int,[Int])]
pInput = pLines pData

-- Part 1

calibration :: (Int,[Int]) -> Bool
calibration (s,[]) = s==0
calibration (s,[x]) = s==x
calibration (s,(x0:x1:xs))
  | s<x0 = False
  | otherwise = calibration (s,(x0*x1:xs)) || calibration (s,(x0+x1:xs))
{-
  any calibration
                         [let (ms,ys) = splitAt i xs in (s,product ms:ys)
                         | i <- [1..length xs]]
-}
  
part1 :: [(Int,[Int])] -> Int
part1 equations = sum $ map fst  (filter calibration equations)

-- Part 2

part2 :: [(Int,[Int])] -> Int
part2 _ = 2

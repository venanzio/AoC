-- Advent of Code 2023, day 1
--  Venanzio Capretta

module Main where

import Data.Maybe (mapMaybe)
import Data.List
import Data.Char

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = lines input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Part 1

getDigit :: Char -> Maybe Int
getDigit c = if isDigit c then Just (read [c]) else Nothing

allDigits :: String -> [Int]
allDigits = mapMaybe getDigit

calibration :: String -> Int
calibration s = let ds = allDigits s in (ds!!0) * 10 + last ds

part1 :: [String] -> Int
part1 = sum . map calibration

-- Part 2

wDigits = ["zero","one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

getWDigit :: String -> Maybe Int
getWDigit "" = Nothing
getWDigit s = case findIndex (\w -> take (length w) s == w) wDigits of
  Just n -> Just n
  Nothing -> getDigit (s!!0)

getAllDigits :: String -> [Int]
getAllDigits = mapMaybe getWDigit . tails

wCalibration :: String -> Int
wCalibration s =
  let digs = getAllDigits s
  in (digs!!0) * 10 + (last digs)
  

part2 :: [String] -> Int
part2 = sum . map wCalibration

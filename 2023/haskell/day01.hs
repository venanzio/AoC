-- Advent of Code 2023, day 1
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
  let xs = lines input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Part 1

calibration :: String -> Int
calibration s = firstDigit s * 10 + firstDigit (reverse s)

firstDigit :: String -> Int
firstDigit (x:xs) = if isDigit x then (read [x])
                                 else firstDigit xs

part1 :: [String] -> Int
part1 = sum . map calibration

-- Part 2

wDigits = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]


readDigit :: String -> Int
readDigit w = (unJust $ elemIndex w wDigits) + 1



bDigit :: String -> Maybe Int
bDigit = bDigitAux wDigits

bDigitAux :: [String] -> String -> Maybe Int
bDigitAux [] s = if isDigit (s!!0) then Just (read [s!!0]) else Nothing
bDigitAux (d:ds) s = if take (length d) s == d
            then Just (readDigit d)
            else bDigitAux ds s

wordDigits :: String -> [Int]
wordDigits [] = []
wordDigits xs = case bDigit xs of
  Just n -> n : wordDigits (tail xs)
  Nothing -> wordDigits (tail xs)

first xs = xs!!0

-- last xs = xs!!(length xs - 1)

wCalibration :: String -> Int
wCalibration s =
  let digs = wordDigits s
  in (first digs) * 10 + (last digs)
  

part2 :: [String] -> Int
part2 = sum . map wCalibration

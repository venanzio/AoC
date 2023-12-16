-- Advent of Code 2023, day 15
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
  putStrLn (show xs)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser String
pData = some (satisfy item (/=','))

pInput :: Parser [String]
pInput = someSepStr pData ","

-- Part 1

hash :: String -> Int
hash = foldl (\h c -> (h+(ord c))*17 `rem` 256) 0

part1 :: [String] -> Int
part1 = sum . map hash

-- Part 2

type Box = [(String,Int)]
type Boxes = [Box]

iboxes :: Boxes
iboxes = take 256 (repeat [])

stepLO :: String -> (String,String)
stepLO = break (`elem` "-=")

removeLens :: String -> Box -> Box
removeLens l box =
  let (ls0,ls1) = break ((==l) . fst) box
  in ls0 ++ tailT ls1

replaceLens :: String -> Int -> Box -> Box
replaceLens l fl box =
  let (l0,l1) = break ((==l) . fst) box
  in l0 ++ (l,fl):tailT l1

stepB :: String -> String -> Box -> Box
stepB l op = case head op of
  '-' -> removeLens l
  '=' -> replaceLens l (read (tail op))
  _   -> error "unknown operation"

step :: String -> Boxes -> Boxes
step s = let (l,op) = stepLO s
         in replaceF (hash l) (stepB l op)

part2 :: [String] -> Int
part2 _ = 2

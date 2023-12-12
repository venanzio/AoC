-- Advent of Code 2023, day 12

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
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser (String,[Int])
pData = do row <- token label
           groups <- someSep natural (char ',')
           return (row,groups)

pInput :: Parser [(String,[Int])]
pInput = pLines pData

-- Part 1

arrangements :: String -> [Int] -> Int
arrangements r gs = sum
  [arrangements0 (drop n r) gs |
   n <- [0 .. length (takeWhile (`elem` ".?") r)] ]

arrangements0 :: String -> [Int] -> Int
arrangements0 r [] = if all (`elem` ".?") r then 1 else 0
arrangements0 r [0] = if all (`elem` ".?") r then 1 else 0
arrangements0 r gs | length r < sum gs + length gs - 1 = 0
arrangements0 r (0:gs) = sum
  [arrangements0 (drop n r) gs |
   n <- [1 .. length (takeWhile (`elem` ".?") r)] ]
arrangements0 r (g:gs) = if length (takeWhile (`elem` "#?") r) >= g
  then arrangements0 (drop g r) (0:gs)
  else 0

part1 :: [(String,[Int])] -> Int
part1 xs = sum [arrangements r gs | (r,gs) <- xs]

-- Part 2

unfold :: (String,[Int]) ->  (String,[Int])
unfold (r,gs) = (concat (r:take 4 (repeat ('?':r))), concat (take 5 (repeat gs)))

part2 :: [(String,[Int])] -> Int
part2  xs = sum [arrangements r gs | (r,gs) <- map unfold xs]

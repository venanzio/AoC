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
arrangements0 r (0:gs) =  sum
  [arrangements0 (drop n r) gs |
   n <- [1 .. length (takeWhile (`elem` ".?") r)] ]
arrangements0 r (g:gs) = if length (takeWhile (`elem` "#?") r) >= g
  then arrangements0 (drop g r) (0:gs)
  else 0

part1 :: [(String,[Int])] -> Int
part1 xs = sum [arrangements r gs | (r,gs) <- xs]

-- Part 2

arrangements2 :: String -> [Int] -> Int
arrangements2 r gs = arrangements r (delete 0 gs)

maxSpring :: String -> (Int,String,String)
maxSpring "" = (0,"","")
maxSpring r =
  let r' = takeWhile (/='#') r
      r0 = drop (length r') r
      r0a = takeWhile (=='#') r0
      n0 = length r0a
      r0b = drop n0 r0
      (n1,r1a,r1b) = maxSpring (drop 1 r0b)
  in if n1 > n0 then (n1,r'++r0a++r1a,r1b) else (n0,r',r0b)

extraSpring :: (Int,String,String) -> (Int,[Int],[Int]) -> Int
extraSpring (n,ra,rb) (g,ga,gb) = sum
  [(arrangements2 ra (i:ga)) * (arrangements2 rb (g-n-i:ga))
  | i <- [0..g-n]]
  
splitSpring :: String -> [Int] -> Int
splitSpring r gs = case maxSpring r of
  (0,r0,_) -> arrangements2 r0 gs
  (n,r0a,r0b) -> undefined
    



unfold :: (String,[Int]) ->  (String,[Int])
unfold (r,gs) = (concat (r:take 4 (repeat ('?':r))), concat (take 5 (repeat gs)))

part2 :: [(String,[Int])] -> Int
part2  xs = sum [arrangements r gs | (r,gs) <- map unfold xs]

-- Advent of Code 2024, day 9
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
-- import Data.Char
import Control.Applicative
-- import qualified Data.Map as M

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
  putStrLn ("Part 2: " ++ show (part2 input))

-- Parsing the input

pInput :: Parser [Int]
pInput = some (digit >>= \d -> return (read [d]))

-- Part 1

diskBlocks :: [Int] -> [Int]
diskBlocks = dBlocks 0 where
  dBlocks n (f:e:map) = take f (repeat n) ++ take e (repeat (-1)) ++ dBlocks (n+1) map
  dBlocks n [f] = take f (repeat n)
  dBlocks _ [] = []


compact :: [Int] -> [Int]
compact = auxCompact . listLW where
  auxCompact blocks
    | isEmptyW blocks = []
    | h  >= 0 = h : auxCompact blocks1
    | isEmptyW blocks1 = []
    | l <0 = auxCompact (insertW (-1) blocks2)
    | otherwise = l : auxCompact blocks2
    where (h,blocks1) = extractW blocks
          (l,blocks2) = extractW (leftW blocks1)

checkSum :: [Int] -> Int
checkSum = sum . zipWith (*) [0..]

part1 :: [Int] -> Int
part1 = checkSum . compact . diskBlocks

-- Part 2

dBlocks :: Int -> String -> [(Int,Int,Int)]
dBlocks n (bl:sp:dmap) = (read [bl],n,read [sp]) : dBlocks (n+1) dmap
dBlocks n [bl] = [(read [bl],n,0)]
dBlocks n [] = []

diskBlocks2 :: String -> [(Int,Int,Int)]
diskBlocks2 = dBlocks 0
{-
dBlocks n dmap@(h:_) = (length bl,n,length sp) : dBlocks (n+1) dmap2 where
  (bl,dmap1) = span (==h) dmap
  (sp,dmap2) = span (==(-1)) dmap
dBlocks _ = []
  -}

bSpace :: (Int,Int,Int) -> Int
bSpace (_,_,sp) = sp

bInsert :: Int -> Int -> [(Int,Int,Int)] -> Maybe [(Int,Int,Int)]
bInsert size f bs
  | bs2 == [] = Nothing
  | otherwise = Just $ bs1 ++ [(s0,f0,0),(size,f,sp0-size)] ++ bs2
  where (bs1,bs2) = span (\b -> bSpace b < size) bs
        (s0,f0,sp0):bs3 = bs2

compact2 :: [(Int,Int,Int)] -> [(Int,Int,Int)]
compact2 bs = case unsnoc bs of
  Nothing -> bs
  Just (bs0,(size,f,_)) -> case (bInsert size f bs0) of
                             Nothing -> bs
                             Just bs1 -> compact2 bs1

checkSum2 :: [(Int,Int,Int)] -> Int
checkSum2 = checkSumAux 0 where
  checkSumAux _ [] = 0
  checkSumAux n ((size,f,sp):bs) =
    sum (map (*f) [n..n+size-1]) + checkSumAux (n+size+sp) bs

part2 :: String -> Int
part2 = checkSum2 . compact2 . diskBlocks2

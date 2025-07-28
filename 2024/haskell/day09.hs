-- Advent of Code 2024, day 9
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Control.Applicative

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

dBlocks :: Int -> [Int] -> [(Int,Int,Int)]
dBlocks n (bl:sp:dmap) = (bl,n,sp) : dBlocks (n+1) dmap
dBlocks n [bl] = [(bl,n,0)]
dBlocks n [] = []

diskBlocks2 :: [Int] -> [(Int,Int,Int)]
diskBlocks2 = dBlocks 0

bSpace :: (Int,Int,Int) -> Int
bSpace (_,_,sp) = sp

bInsert :: Int -> Int -> [(Int,Int,Int)] -> Maybe [(Int,Int,Int)]
bInsert size f bs
  | bs2 == [] = Nothing
  | otherwise = Just $ bs1 ++ [(s0,f0,0),(size,f,sp0-size)] ++ bs3
  where (bs1,bs2) = span (\b -> bSpace b < size) bs
        (s0,f0,sp0):bs3 = bs2

compact2 :: [(Int,Int,Int)] -> [(Int,Int,Int)]
compact2 bs = case unsnoc bs of
  Nothing -> bs
  Just (bs0,b@(size,f,sp)) ->
    case (bInsert size f bs0) of
      Nothing -> compact2 bs0 ++ [b]
      Just bs1 -> let Just (bs2,(size2,f2,sp2)) = unsnoc bs1
                  in compact2 (bs2 ++ [(size2,f2,sp2+size+sp)])

checkSum2 :: [(Int,Int,Int)] -> Int
checkSum2 = checkSumAux 0 where
  checkSumAux _ [] = 0
  checkSumAux n ((size,f,sp):bs) =
    sum (map (*f) [n..n+size-1]) + checkSumAux (n+size+sp) bs

part2 :: [Int] -> Int
part2 = checkSum2 . compact2 . diskBlocks2

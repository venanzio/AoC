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
  let bs = diskBlocks $ parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 bs))
  putStrLn ("Part 2: " ++ show (part2 bs))

-- Parsing the input

pInput :: Parser [Int]
pInput = some (digit >>= \d -> return (read [d]))

dBlocks :: Int -> [Int] -> [(Int,Int,Int)]
dBlocks n (bl:sp:dmap) = (bl,n,sp) : dBlocks (n+1) dmap
dBlocks n [bl] = [(bl,n,0)]
dBlocks n [] = []

diskBlocks :: [Int] -> [(Int,Int,Int)]
diskBlocks = dBlocks 0

-- Part 1

compact1 :: [(Int,Int,Int)] -> [(Int,Int,Int)]
compact1 bs = case unsnoc bs of
  Nothing -> []
  Just (bs0,(size,f,sp)) -> bInsert1 size f bs0

bInsert1 :: Int -> Int -> [(Int,Int,Int)] -> [(Int,Int,Int)]
bInsert1 size f [] = [(size,f,0)]
bInsert1 size f ((s0,f0,sp):bs)
  | sp == 0 = (s0,f0,0) : bInsert1 size f bs
  | sp < size = (s0,f0,0) : (sp,f,0) : bInsert1 (size-sp) f bs
  | sp == size = (s0,f0,0) : (size,f,0) : compact1 bs
  | sp > size = (s0,f0,0) : compact1 ((size,f,sp-size):bs)

checkSum :: [(Int,Int,Int)] -> Int
checkSum = checkSumAux 0 where
  checkSumAux _ [] = 0
  checkSumAux n ((size,f,sp):bs) =
    sum (map (*f) [n..n+size-1]) + checkSumAux (n+size+sp) bs

part1 :: [(Int,Int,Int)] -> Int
part1 = checkSum . compact1

-- Part 2

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

part2 :: [(Int,Int,Int)] -> Int
part2 = checkSum . compact2

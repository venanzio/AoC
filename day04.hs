-- Advent of Code 2021, day 4

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let (nums,bs) = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 nums bs))
  putStrLn ("Part 2: " ++ show (part2 nums bs))

-- Parsing the input

pNumbers = seqSep natural ","

data Cell = Free Int | Marked Int
  deriving Show

type Board = [[Cell]]

pBoard :: Parser Board
pBoard = do cls <- repN 5 (repN 5 natural)
            return (map (map Free) cls)
            
pData :: Parser ()
pData = return ()

pInput :: Parser ([Int], [Board])
pInput = do numbers <- pNumbers
            boards <- some pBoard
            return (numbers,boards)

-- Part 1

isMarked (Free _) = False
isMarked (Marked _) = True

cellNum :: Cell -> Int
cellNum (Free x) = x
cellNum (Marked x) = x

checkRow :: Board -> Int -> Bool
checkRow b i = all isMarked (b!!i)

checkRows b = or [checkRow b i | i <- [0..4]]

checkCol :: Board -> Int -> Bool
checkCol b i = all isMarked (map (!!i) b)

checkCols b = or [checkCol b i | i <- [0..4]]

checkBoard :: Board -> Bool
checkBoard b = checkRows b || checkCols b

mapBoard :: (Cell -> Cell) -> Board -> Board
mapBoard f b = map (map f) b

callNum :: Int -> Board -> Board
callNum y = mapBoard (\c -> if (cellNum c) == y then Marked y else c)

callN :: Int -> [Board] -> [Board]
callN y = map (callNum y)

bingo :: [Int] -> [Board] -> Maybe (Int,Board)
bingo []  _     = Nothing
bingo (x:xs) bs = let bs' = callN x bs
                      wb = filter checkBoard bs'
                  in  case wb of
                        [] -> bingo xs bs'
                        (b:_) -> Just (x,b)

free :: Cell -> Int
free (Free x) = x
free _ = 0

sumFree :: Board -> Int
sumFree b = sum (map sum (map (map free) b))
  
part1 :: [Int] -> [Board] -> Int
part1 xs bs = let Just (x,b) = bingo xs bs in (sumFree b)*x

-- Part 2

lastBingo ::  [Int] -> [Board] -> Maybe (Int,Board)
lastBingo []  _     = Nothing
lastBingo (x:xs) bs =
  let bs' = filter (not . checkBoard) (callN x bs)
  in  case bs' of
        [b] -> if checkBoard b then Just (x,b) else bingo xs bs'
        (b:_) -> lastBingo xs bs'

part2 :: [Int] -> [Board] -> Int
part2 xs bs = let Just (x,b) = lastBingo xs bs in (sumFree b)*x


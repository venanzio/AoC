-- Advent of Code 2021, day 13

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
  let (p,fs) = parseAll pInput input
  putStrLn ("paper: " ++ show (last p))
  putStrLn ("folds: " ++ show fs)
  putStrLn ("Part 1: " ++ show (part1 p fs))
  putStrLn ("Part 2: \n" ++ (part2 p fs))

-- Parsing the input

type Paper = [(Int,Int)]

data Fold = Vertical Int | Horizontal Int
  deriving (Eq,Show)
  
pDot :: Parser (Int,Int)
pDot = do x <- natural
          string ","
          y <- natural
          return (x,y)

pDots :: Parser [(Int,Int)]
pDots = many pDot

pPaper :: Parser Paper
pPaper = pDots

pFold :: Parser Fold
pFold = (string "fold along y=" >> natural >>= return.Vertical) <|>
        (string "fold along x=" >> natural >>= return.Horizontal)
           

pInput :: Parser (Paper,[Fold])
pInput = do p <- pPaper
            fs <- many pFold
            return (p,fs)

-- Part 1

foldDot :: Fold -> (Int,Int) -> (Int,Int)
foldDot (Vertical y) (a,b) = (a, if b>y then 2*y-b else b)
foldDot (Horizontal x) (a,b) = (if a>x then 2*x-a else a, b)


fold :: Paper -> Fold -> Paper
fold p f = nub $ map (foldDot f) p

part1 :: Paper -> [Fold] -> Int
part1 p fs = length $ fold p (head fs)

-- Part 2

showPaper :: Paper -> String
showPaper p = concat [[showDot (a,b) | a <- [0..maxX]] ++ "\n" | b <- [0..maxY]]
  where maxX = maximum (map fst p)
        maxY = maximum (map snd p)
        showDot c = if c `elem` p then '#' else '.'

part2 :: Paper -> [Fold] -> String
part2 p fs = showPaper (foldl fold p fs)

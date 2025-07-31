-- Advent of Code 2024, day 13
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
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
  let ms = parseAll pInput input
  putStrLn (show (solveMachine (ms!!3)))
  putStrLn ("Part 1: " ++ show (part1 ms))
  putStrLn ("Part 2: " ++ show (part2 ms))

-- Parsing the input

data Machine = Mac { xA :: Int, yA :: Int,
                     xB :: Int, yB :: Int,
                     xP :: Int, yP :: Int }
  deriving (Eq,Show)

pButton :: Char -> Parser (Int,Int)
pButton c = do symbol "Button"
               char c
               symbol ":"
               symbol "X+"
               x <- natural
               symbol ","
               symbol "Y+"
               y <- natural
               return (x,y)

pPrize :: Parser (Int,Int)
pPrize = do symbol "Prize:"
            symbol "X="
            x <- natural
            symbol ","
            symbol "Y="
            y <- natural
            return (x,y)

pMachine :: Parser Machine
pMachine = do (ax,ay) <- pButton 'A'
              (bx,by) <- pButton 'B'
              (px,py) <- pPrize
              return (Mac ax ay bx by px py)

pInput :: Parser [Machine]
pInput = some pMachine

-- Part 1

solveMachine :: Machine -> [(Int,Int)]
solveMachine m
  | da == 0 = error "solve single diophantine equation"
  | ra == 0 && rb == 0 = [(a,b)]
  | otherwise = []
  where da = yB m * xA m - xB m * yA m
        (a,ra) = quotRem (yB m * xP m - xB m * yP m) da
        (b,rb) = quotRem (xP m - xA m * a) (xB m)

part1 :: [Machine] -> Int
part1 ms = sum [3*a+b | m <- ms, (a,b) <- solveMachine m]

-- Part 2

bigMachine :: Machine -> Machine
bigMachine m = m { xP = xP m + 10000000000000, yP = yP m + 10000000000000 }

part2 :: [Machine] -> Int
part2 = part1 . map bigMachine

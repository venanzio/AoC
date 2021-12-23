-- Advent of Code 2021, day 23

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
data Antiphod = A | B | C | D | E
  deriving (Eq,Show)
type Burrow = [[Antiphod]]

pSkip :: Parser ()
pSkip = many (sat (\c -> c`elem` ".#" || isSpace c)) >> return ()

pAntiphod :: Parser Antiphod
pAntiphod = do pSkip
               (char 'A' >> return A) <|>
                 (char 'B' >> return B) <|>
                   (char 'C' >> return C) <|>
                     (char 'D' >> return D)

pRooms :: [[Antiphod]] -> Parser [[Antiphod]]
pRooms [r1,r2,r3,r4] =
  (pAntiphod >>= \a -> pRooms [r2,r3,r4,r1++[a]]) <|> (pSkip >> return [r1,r2,r3,r4])

pInput :: Parser Burrow
pInput = pRooms  [[],[],[],[]] >>= return . (take 11 (repeat E) :)

-- Part 1

part1 :: Burrow -> Int
part1 _ = 1

-- Part 2

part2 :: Burrow -> Int
part2 _ = 2

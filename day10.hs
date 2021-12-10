-- Advent of Code 2021, day 10

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

pData :: Parser ()
pData = return ()

pInput :: Parser [String]
pInput = pLines (some item)

-- Part 1

data Error = Complete | Incomplete String | Corrupted Char
  deriving (Eq,Show)

matches = [('(',')'),('[',']'),('{','}'),('<','>')]

match c = let Just c' = lookup c matches in c'

ops = map fst matches
cls = map snd matches


parMatch :: String -> String -> Error
parMatch [] [] = Complete
parMatch [] stack = Incomplete stack
parMatch (c:cs) stack
  | c `elem` ops = parMatch cs (match c : stack)
parMatch (c:cs) (c':stack)
  | c `elem` cls = if c == c' then parMatch cs stack
                              else Corrupted c
parMatch _ _ = error "empty stack"

score :: String -> Int
score cs = case parMatch cs [] of
  Corrupted ')' -> 3
  Corrupted ']' -> 57
  Corrupted '}' -> 1197
  Corrupted '>' -> 25137
  _ -> 0

part1 :: [String] -> Int
part1 = sum . map score


-- Part 2

scoreC :: String -> Int
scoreC = foldl (\y x -> 5*y + scr x ) 0
  where scr ')' = 1
        scr ']' = 2
        scr '}' = 3
        scr '>' = 4

scoreLine :: String -> Maybe Int
scoreLine xs = case parMatch xs [] of
  Incomplete stack -> Just (scoreC stack)
  _ -> Nothing

part2 :: [String] -> Int
part2 cs = undefined

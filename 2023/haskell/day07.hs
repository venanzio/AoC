-- Advent of Code 2023, day 7
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
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

pData :: Parser (String,Int)
pData = do hand <- word
           bid <- natural
           return (hand,bid)

pInput :: Parser [(String,Int)]
pInput = pLines pData

-- Part 1

cards = "23456789TJQKA"

handType :: String -> [Int]
handType hand = reverse $ sort $ map (\card -> occurrences card hand) cards

cardRank :: String -> Char -> Int
cardRank cs card = unJust $ elemIndex card cs

handRank :: String -> ([Int],[Int])
handRank hand = (handType hand, map (cardRank cards) hand)

-- Winnings (with hands already sorted by rank)
winnings :: [(String,Int)] -> Int
winnings xs =  sum $ map (\(x,i) -> i*(snd x)) $ zip xs [1..]

part1 :: [(String,Int)] -> Int
part1 xs = winnings (sortOn (handRank.fst) xs)

-- Part 2

cardsJ = "J23456789TQKA"

handTypeJ :: String -> [Int]
handTypeJ "JJJJJ" = [5]
handTypeJ hand =
  let t = handType (deleteAll 'J' hand)
  in (t!!0 + occurrences 'J' hand) : tail t

handRankJ :: String -> ([Int],[Int])
handRankJ hand = (handTypeJ hand, map (cardRank cardsJ) hand)

part2 :: [(String,Int)] -> Int
part2 xs =  winnings (sortOn (handRankJ.fst) xs)


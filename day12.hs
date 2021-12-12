-- Advent of Code 2021, day 12

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
  putStrLn (show $ head $ paths xs)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser (String,String)
pData = do s <- token (many alphanum)
           string "-"
           t <- token (many alphanum)
           return (s,t)
           
pInput :: Parser [(String,String)]
pInput = some pData

-- Part 1

caves :: [(String,String)] -> [String]
caves = nub . foldl (\cs (s,t) -> s:t:cs) []

bigCaves :: [String] -> [String]
bigCaves = filter (isUpper.head)

small :: String -> Bool
small cave = isLower (head cave)

smallCaves :: [String] -> [String]
smallCaves = filter (isLower.head)

edges :: [(String,String)] -> [(String,String)]
edges xs = [(s,t) | (s,u) <- xs, isLower (head s), (u',t) <- xs, u==u']


connected :: [(String,String)] -> String -> [String]
connected edges s = map snd (filter ((==s).fst) edges) ++ map fst (filter ((==s).snd) edges)

pathNext :: [(String,String)] -> [String] -> [[String]]
pathNext edges p = [p++[t] | t <- connected edges (last p), not (small t && t `elem` p)]

pathExtend :: [(String,String)] -> [String] -> [[String]]
pathExtend edges p = [p] ++ (pathNext edges p >>= pathExtend edges)

paths :: [(String,String)] -> [[String]]
paths edges = pathExtend edges ["start"]

part1 :: [(String,String)] -> Int
part1 edges = length (filter ((=="end").last) (paths edges))

-- Part 2

dropLast l = take (length l -1) l

duplicates l = nub [s | s <- l, length (filter (==s) l) > 1]

vPath :: [(String,String)] -> [String] -> Bool
vPath edges p =
  let small = filter (isLower.head) p
      dups = duplicates small -- [s | s <- small, length (filter (==s) small) > 1]
  in (length dups <= 1) && (not ("end" `elem` dropLast p))

pNext :: [(String,String)] -> [String] -> [[String]]
pNext edges p = [p++[t] | t <- connected edges (last p), vPath edges (p++[t])]

pExtend :: [(String,String)] -> [String] -> [[String]]
pExtend edges p = [p] ++ (pNext edges p >>= pExtend edges)

paths2 :: [(String,String)] -> [[String]]
paths2 edges = pExtend edges ["start"]

part2 :: [(String,String)] -> Int
part2 edges = length (filter ((=="end").last) (paths2 edges))

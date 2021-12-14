-- Advent of Code 2021, day 14

-- IDEA: solution is quite messy, needs revising

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
  let (t,rs) = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 t rs))
  putStrLn ("Part 2: " ++ show (part2 t rs))

-- Parsing the input

type Template = String
type Rule = ((Char,Char),Char)


pTemplate :: Parser Template
pTemplate = many letter

pRule :: Parser Rule
pRule = do a <- letter
           b <- letter
           token (string "->")
           c <- letter
           return ((a,b),c)

pInput :: Parser (Template,[Rule])
pInput = do t <- pTemplate
            rs <- many (token pRule)
            return (t,rs)

-- Part 1

insertion :: [Rule] -> Template -> Template
insertion rs (a:b:t) = case lookup (a,b) rs of
                         Just c -> a:c:insertion rs (b:t)
                         Nothing -> a:insertion rs (b:t)
insertion _ t = t

count :: Template -> Char -> Int
count t c = length (filter (==c) t)

final :: Template -> [Rule] -> Int -> Int
final t rs n =
  let t' = nIter (insertion rs) n t
      cs = nub t'
      (_,_,mx) = maximumF (count t') cs
      (_,_,mn) = minimumF (count t') cs
  in (mx-mn)

part1 :: Template -> [Rule] -> Int
part1 t rs = final t rs 10
  
-- Part 2

type Templ = M.Map (Char,Char) Int

type Count = M.Map Char Int

templ :: Template -> Templ
templ t = foldr (\ab -> M.insertWith (+) ab 1) M.empty (tPairs t)

tCounts :: Template -> Count
tCounts = foldr (\c -> M.insertWith (+) c 1) M.empty 

tPairs :: Template -> [(Char,Char)]
tPairs (a:b:t) = (a,b) : tPairs (b:t)
tPairs _ = []

rule :: Templ -> Rule -> (Templ,Count) -> (Templ,Count)
rule m0 ((a,b),c) (m,count) = case M.lookup (a,b) m0 of
  Nothing -> (m,count)
  Just x -> let m' = M.insertWith (+) (a,c) x $
                     M.insertWith (+) (c,b) x $
                     M.adjust (\y -> y-x) (a,b) m
                counts' = M.insertWith (+) c x count
            in (m',counts')

tInsert :: [Rule] -> (Templ,Count) -> (Templ,Count)
tInsert rs (m,counts) = foldr (rule m) (m,counts) rs

letters :: [Rule] -> [Char]
letters = nub . foldl (\ls ((a,b),c) -> a:b:c:ls) []

tCount :: Count -> Char -> Int
tCount counts c = counts M.! c

tFinal :: (Templ,Count) -> [Rule] -> Int -> Int
tFinal mcs rs n =
  let (_,counts') = nIter (tInsert rs) n mcs
      cs = letters rs
      (_,_,mx) = maximumF (tCount counts') cs
      (_,_,mn) = minimumF (tCount counts') cs
  in (mx-mn)

part2 ::  Template -> [Rule] -> Int
part2 t rs = tFinal (templ t, tCounts t) rs 40

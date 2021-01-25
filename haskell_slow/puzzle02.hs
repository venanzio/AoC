{- Day 2
   For now just copied the quick solutions
-}

module PuzzleInput where

import System.Environment
import Data.List

import FunParser
import Control.Applicative

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let its = parseAll entries input
  putStrLn ("Part 1: " ++ show (count testEntry its))
  putStrLn ("Part 2: " ++ show (count testE its))

-- Parsing the entries
entries :: Parser [(Int,Int,Char,String)]
entries = some entry

-- An example entry is "1-3 a: abcde" saying that
--  the password is "abcde" and should contain between 1 and 3 'a's
entry :: Parser (Int,Int,Char,String)
entry = do
  min <- natural
  symbol "-"
  max <- natural
  c <- token item  -- item is just a Char, token eliminates white spaces
  symbol ":"
  word <- token label  -- label is any string with not white spaces
  return (min,max,c,word)


-- For part 1

occurrences :: Eq a => a -> [a] -> Int
occurrences x ys = foldl  (\n y -> n + if y==x then 1 else 0) 0 ys

testEntry :: (Int,Int,Char,String) -> Bool
testEntry (min,max,c,w) =
  let n = occurrences c w in min <= n && n <= max

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- For part 2

indx :: Int -> [a] -> a
indx n l = l!!(n-1)

testE :: (Int,Int,Char,String) -> Bool
testE (min,max,c,w) = (indx min w == c) `xor` (indx max w == c)

xor :: Bool -> Bool -> Bool
xor = (/=)

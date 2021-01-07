module PuzzleInput where

import System.Environment
import Data.List

import FunParser
import Control.Applicative

puzzle :: String -> IO Int
puzzle fileName = do
  input <- readFile fileName
  let its = parseAll entries input  
  return (count testE its)

  
entries :: Parser [(Int,Int,Char,String)]
entries = some entry

entry :: Parser (Int,Int,Char,String)
entry = do
  min <- natural
  symbol "-"
  max <- natural
  c <- token item
  symbol ":"
  word <- token label
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

rangeOcc :: Eq a => Int -> Int -> a -> [a] -> Int
rangeOcc min max x ys = occurrences x (take (max-min+1) (drop (min-1) ys))

indx :: Int -> [a] -> a
indx n l = l!!(n-1)

countIdx :: Eq a => a -> [a] -> [Int] -> Int
countIdx x ys idx = foldl (\ n i -> n + if (indx i ys)==x then 1 else 0)  0 idx

testE :: (Int,Int,Char,String) -> Bool
testE (min,max,c,w) = countIdx c w [min,max] == 1

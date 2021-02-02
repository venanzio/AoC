-- Advent of Code 2020, day 5

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let grps = parseAll groupsComm input
  putStrLn ("Part 2: " ++ show (sum grps))

-- Parsing

grAns :: Parser [[String]]
grAns = blocks (some line)


-- part 1

noDups :: Eq a => [a] -> [a]
noDups = foldr (\x ys -> if x `elem` ys then ys else x:ys) []

groupCount :: Parser Int
groupCount = some (token item) >>= return . length . noDups
                
groups :: Parser [Int]
groups = blocks groupCount

-- part 2

common :: Eq a => [[a]] -> [a]
common [] = []
common (xs:xss) = noDups $ foldl intersect xs xss

groupCommon :: Parser Int
groupCommon = do ps <- (some line)
                 return $ length (common ps)

groupsComm :: Parser [Int]
groupsComm = blocks groupCommon



module PuzzleInput where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

puzzle :: String -> IO Int
puzzle fileName = do
  input <- readFile fileName
  let grps = parseAll groupsComm input
  return (sum grps)

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



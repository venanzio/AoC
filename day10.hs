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

data Error = Complete | Incomplete | Corrupted Char
  deriving (Eq,Show)

matches = [('(',')'),('[',']'),('{','}'),('<','>')]

ops = map fst matches
cls = map snd matches

chunkE :: Parser Error
chunkE = do c <- choice (map char ops)
            e <- chunksE
            case e of
              Complete -> do c' <- choice (map char cls)
                             if (c,c') `elem` matches then return Complete else return (Corrupted c')
                          <|> return Incomplete
              e' -> return e'

chunksE :: Parser Error
chunksE = do e <- chunkE
             case e of
               Complete -> chunksE
               e -> return e
          <|> return Complete


score :: String -> Int
score s = let ((e,_):_) = parse chunksE s in case e of
  Corrupted ')' -> 3
  Corrupted ']' -> 57
  Corrupted '}' -> 1197
  Corrupted '>' -> 25137
  _ -> 0

part1 :: [String] -> Int
part1 = sum . map score

-- Part 2

part2 :: [String] -> Int
part2 _ = 2

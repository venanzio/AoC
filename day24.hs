-- Advent of Code 2021, day 24

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

data Var = W | X | Y | Z
  deriving (Show,Eq)
data Instruction = Inp Var
                 |  Add Var (Either Var Int)
                 |  Mul Var (Either Var Int)
                 |  Div Var (Either Var Int)
                 |  Mod Var (Either Var Int)
                 |  Eql Var (Either Var Int)
  deriving (Show,Eq)

pVar :: Parser Var
pVar = (symbol "w" >> return W) <|>
       (symbol "x" >> return X) <|>
       (symbol "y" >> return Y) <|>
       (symbol "z" >> return Z)

pArg :: Parser (Either Var Int)
pArg = (pVar >>= return . Left) <|> (integer >>= return . Right)

pBin :: String -> (Var -> Either Var Int -> Instruction) -> Parser Instruction
pBin name const =
  do symbol name
     a <- pVar
     b <- pArg
     return (const a b)

pInst :: Parser Instruction
pInst =
  (symbol "inp" >> pVar >>= return . Inp) <|>
  (pBin "add" Add) <|>
  (pBin "mul" Mul) <|>
  (pBin "div" Div) <|>
  (pBin "mod" Mod) <|>
  (pBin "eql" Eql)
  
pInput :: Parser [Instruction]
pInput = pLines pInst

-- Part 1

part1 :: [Instruction] -> Int
part1 _ = 1

-- Part 2

part2 :: [Instruction] -> Int
part2 _ = 2

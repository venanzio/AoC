-- Advent of Code 2024, day 17
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
-- import Data.Char
-- import Control.Applicative
-- import qualified Data.Map as M

import FunParser
-- import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let (regs,prog) = parseAll pInput input
  putStrLn (show regs)
  putStrLn (show prog)
  putStrLn ("Part 1: " ++ show (part1 regs prog))
  putStrLn ("Part 2: " ++ show (part2 regs prog))

-- Parsing the input

pRegister :: Char -> Parser Int
pRegister r = symbol ("Register "++r:":") >> natural

pRegisters :: Parser (Int,Int,Int)
pRegisters = do rA <- pRegister 'A'
                rB <- pRegister 'B'
                rC <- pRegister 'C'
                return (rA,rB,rC)

pProgram :: Parser [Int]
pProgram = symbol "Program:" >> someSep natural (symbol ",")

pInput :: Parser ((Int,Int,Int),[Int])
pInput = do regs <- pRegisters
            prog <- pProgram
            return (regs,prog)

-- Part 1

part1 :: (Int,Int,Int) -> [Int] -> Int
part1 regs prog = 1

-- Part 2

part2 :: (Int,Int,Int) -> [Int] -> Int
part2 regs prog = 2

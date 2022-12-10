-- Advent of Code 2022, day 10
--  Venanzio Capretta

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
  part2 xs

-- Parsing the input

data Inst = Noop | Addx Int | NextAddx Int
  deriving (Show,Eq)

pData :: Parser Inst
pData = do symbol "noop"
           return Noop
        <|>
        do symbol "addx"
           x <- integer
           return (Addx x)

pInput :: Parser [Inst]
pInput = many pData

-- Part 1

cycleTest :: Int -> Bool
cycleTest cycle = (cycle - 20) `mod` 40 == 0

strUp :: Int -> Int -> Int -> Int
strUp cycle reg str =
  if cycleTest cycle then str + cycle*reg else str

exec :: [Inst] -> (Int,Int,Int) -> Int
exec [] (cycle,reg,str) = str
exec (Noop:insts) (cycle,reg,str) =
   exec insts (cycle+1, reg, strUp cycle reg str)
exec (Addx x:insts) (cycle,reg,str) =
   exec (NextAddx x:insts) (cycle+1, reg, strUp cycle reg str)
exec (NextAddx x:insts) (cycle,reg,str) =
   exec insts (cycle+1, reg+x, strUp cycle reg str)

part1 :: [Inst] -> Int
part1 insts = exec insts (1,1,0)

-- Part 2

pixel :: Int -> Int -> Char
pixel cycle reg =
  if abs ((cycle-1) `mod` 40 - reg) <= 1 then '#' else '.'

execCRT :: [Inst] -> (Int,Int) -> String
execCRT [] (cycle,reg) = ""
execCRT (Noop:insts) (cycle,reg) =
   pixel cycle reg : execCRT insts (cycle+1, reg)
execCRT (Addx x:insts) (cycle,reg) =
   pixel cycle reg : execCRT (NextAddx x:insts) (cycle+1, reg)
execCRT (NextAddx x:insts) (cycle,reg) =
   pixel cycle reg : execCRT insts (cycle+1, reg+x)

printCRT :: String -> IO ()
printCRT "" = return ()
printCRT crt = do let (ln,crt') = splitAt 40 crt
                  putStrLn ln
                  printCRT crt'
                  
part2 :: [Inst] -> IO ()
part2 insts = printCRT (execCRT insts (1,1))

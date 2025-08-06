-- Advent of Code 2024, day 17
--  Venanzio Capretta

module Main where

import System.Environment
import Data.Bits
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

readR :: Char -> (Int,Int,Int) -> Int
readR 'A' (a,b,c) = a
readR 'B' (a,b,c) = b
readR 'C' (a,b,c) = c

writeR :: Char -> Int -> (Int,Int,Int) -> (Int,Int,Int)
writeR 'A' a (_,b,c) = (a,b,c)
writeR 'B' b (a,_,c) = (a,b,c)
writeR 'C' c (a,b,_) = (a,b,c)

combo :: Int -> (Int,Int,Int) -> Int
combo 4 = readR 'A'
combo 5 = readR 'B'
combo 6 = readR 'c'
combo 7 = \_ -> error "invalid combo operand 7"
combo n = \_ -> 2


runProg :: [Int] -> Int -> (Int,Int,Int) -> [Int]
runProg prog pointer reg = if pointer >= progL then []
   else case opcode of
          0 -> continue (writeR 'A' rdiv reg) -- adv
          1 -> continue (writeR 'B' (readR 'B' reg `xor` lop) reg) -- bxl
          2 -> continue (writeR 'B' (cop `mod` 8) reg) -- bst
          3 -> case (readR 'A' reg) of -- jnz
                 0 -> continue reg
                 p -> jump p
          _ -> undefined
  where progL = length prog
        opcode  = prog!!pointer
        lop = prog!!(pointer+1)
        cop = combo lop reg
        pointer' = pointer+2
        continue = runProg prog pointer'
        jump p = runProg prog p reg
        rdiv = readR 'A' reg `div` (2^cop)

part1 :: (Int,Int,Int) -> [Int] -> Int
part1 regs prog = 1

-- Part 2

part2 :: (Int,Int,Int) -> [Int] -> Int
part2 regs prog = 2

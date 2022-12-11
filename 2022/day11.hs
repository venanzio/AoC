-- Advent of Code 2022, day 11
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
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

data Monkey = Monkey {
  mItems :: [Int],
  mOperation :: Int -> Int,
  mTest :: Int -> Int,  -- indicating to which monkey to throw
  mActivity :: Int
  }

filterDiv :: Int -> [Int] -> [Int]
filterDiv x = filter (\y -> y `mod` x /= 0)

sieve :: [Int] -> [Int]
sieve (x:ys) = x : sieve (filterDiv x ys)

primes :: [Int]
primes = sieve [2..]


bigNum = product (take 30 primes)

type Monkeys = M.Map Int Monkey

pOp :: Parser (Int -> Int -> Int)
pOp = (symbol "+" >> return (+)) <|> (symbol "*" >> return (*)) 

pOperand :: Parser (Int -> Int)
pOperand = (symbol "old" >> return id)
           <|>
           do x <- natural
              return (\_ -> x)

pOperation :: Parser (Int -> Int)
pOperation = do a1 <- pOperand
                op <- pOp
                a2 <- pOperand
                return (\x -> op (a1 x) (a2 x))

pMonkeyN :: Parser Int
pMonkeyN = do symbol "Monkey"
              n <- natural
              symbol ":"
              return n

pMonkey :: Parser Monkey
pMonkey =
  do symbol "Starting items:"
     items <- manySepStr natural ","
     symbol "Operation: new ="
     operation <- pOperation
     symbol "Test: divisible by"
     testV <- natural
     symbol "If true: throw to monkey"
     monkey1 <- natural
     symbol "If false: throw to monkey"
     monkey2 <- natural
     return (Monkey {mItems = items,
                     mOperation = operation,
                     mTest = \x -> if x `mod` testV == 0
                                     then monkey1 else monkey2,
                     mActivity = 0})
              
pMonkeys :: Parser Monkeys
pMonkeys = do n <- pMonkeyN
              m <- pMonkey
              ms <- pMonkeys
              return (M.insert n m ms)
           <|> return M.empty

pInput :: Parser Monkeys
pInput = pMonkeys

-- Part 1

monkeyThrow :: Int -> Monkeys -> Monkeys
monkeyThrow n ms =
  let m = ms M.! n
  in case mItems m of
    [] -> ms
    (x:xs) -> let x' = mOperation m x `mod` bigNum -- `div` 3
                  n1 = (mTest m x')
                  m1 = ms M.! n1
              in M.insert n (m {mItems = xs, mActivity = mActivity m +1})
                   (M.insert n1 (m1 {mItems = x':mItems m1}) ms)

monkeyTurn :: Int -> Monkeys -> Monkeys
monkeyTurn n ms = if (mItems (ms M.! n)) == []
                    then ms
                    else monkeyTurn n (monkeyThrow n ms)

monkeyRound :: Monkeys -> Monkeys
monkeyRound ms = M.foldlWithKey (\ms n _ -> monkeyTurn n ms) ms ms

monkeyBusiness :: Monkeys -> Int
monkeyBusiness ms =
  let as = reverse $ sort $ M.elems $ M.map mActivity ms
  in as!!0 * as!!1
  
part1 :: Monkeys -> Int
part1 = monkeyBusiness . nIter monkeyRound 20

-- Part 2

part2 :: Monkeys -> Int
part2 = monkeyBusiness . nIter monkeyRound 10000


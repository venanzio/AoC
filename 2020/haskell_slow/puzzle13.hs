-- Advent of Code 2020, day 13

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
  let (t,bts) = parseAll pInput input
      bs = map fst bts
  putStrLn (show bts)
  putStrLn ("Part 1: " ++ show (part1 t bs))
  putStrLn ("Part 2: " ++ show (part2 bts))

-- Parsing the input

pBus :: Parser (Maybe Int)
pBus = (natural >>= return.Just) <|> (symbol "x" >> return Nothing)

-- bus and arrival time
busTimes :: [Maybe Int] -> [(Int,Int)]
busTimes bs = delNothing (zip bs [0..])
  where delNothing [] = []
        delNothing ((Nothing,_):bts) = delNothing bts
        delNothing ((Just b,t):bts) = (b,t):delNothing bts

pBusses :: Parser [(Int,Int)]
pBusses = (seqSep pBus ",") >>= return.busTimes  -- return.filterJust

pInput :: Parser (Int,[(Int,Int)])
pInput = do time <- natural
            busses <- pBusses
            return (time,busses)

-- Part 1

-- for a given bus, return the next departure time after t
busTime :: Int -> Int -> Int
busTime t b = let r = t `rem` b
              in if r==0 then t else t+b-r

-- first bus to depart and time of departure
firstBus :: Int -> [Int] -> (Int,Int)
firstBus t bs = let (_,b,tb) = minimumF (busTime t) bs in (b,tb)

part1 :: Int -> [Int] -> Int
part1 t bs = let (b,t') = (firstBus t bs) in b * (t'-t)

-- Part 2

-- If the numbers in the list are coprime, we can use the Chinese Remainder Theorem

-- Are two numbers relatively prime?
relPrime :: Int -> Int -> Bool
relPrime x y = gcd x y == 1

-- Are numbers in a list pairways relatively prime?
coPrimes :: [Int] -> Bool
coPrimes (x:xs) = all (relPrime x) xs && coPrimes xs
coPrimes [] = True

-- Extended Euclidean algorithm: returns the coefficients and the gcd
euclid :: Int -> Int -> (Int,Int,Int)
euclid x 0 = (1,0,x)
euclid x y = let d = x `div` y
                 r = x `mod` y
                 (a,b,g) = euclid y r
             in (b,a-b*d,g)

-- Chinese Reminder for two moduli:
--  if n1 n2 are relatively prime, find x s.t.
--  x = a1 (mod n1), x = a2 (mod n2)
chinesePair :: (Int,Int) -> (Int,Int) -> Int
chinesePair (n1,a1) (n2,a2) =
  let (m1,m2,g) = euclid n1 n2
  in if g/=1 then error ("moduli " ++ show n1 ++ " and " ++ show n2 ++ " not relatively prime")
             else a1*m2*n2 + a2*m1*n1

-- Chinese reminder coefficient for a list of modulus/reminder
chinese :: [(Int,Int)] -> Int
chinese [(n,a)] = a
chinese ((n1,a1):(n2,a2):ps) = chinese ((n1*n2,chinesePair (n1,a1) (n2,a2)):ps) 
                               

part2 :: [(Int,Int)] -> Int
part2 = chinese . map (\(b,t) -> (b,-t))

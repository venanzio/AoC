-- Advent of Code 2024, day 10
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
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
  let map = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 map))
  putStrLn ("Part 2: " ++ show (part2 map))

-- Parsing the input

pData :: Parser [Int]
pData = some (digit >>= return . read . singleton)

pInput :: Parser (Map2D Int)
pInput = pLines pData >>= return . mMap

-- Part 1

dirs = [dUp,dDown,dLeft,dRight]

trails :: Map2D Int -> Point -> [Point]
trails m p = if ph == 9 then [p] else concat $ map (trails m0) neighbours
  where ph = m M.! p
        m0 = M.delete p m
        neighbours = filter (\q -> M.lookup q m0 == Just (ph+1))
                            (map (pMove p) dirs)

part1 :: (Map2D Int) -> Int
part1 m = sum (map (length . nub . trails m) (mFind 0 m))

-- Part 2

part2 :: (Map2D Int) -> Int
part2 m = sum (map (length . trails m) (mFind 0 m))

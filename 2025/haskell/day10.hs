-- Advent of Code 2025, day 10
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
  let ms = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 ms))
  putStrLn ("Part 2: " ++ show (part2 ms))

-- Parsing the input

type Machine = ([Bool],[[Int]],[Int])

pLights :: Parser [Bool]
pLights = delim "[" (some light) "]" where
  light = (symbol "." >> return False) <|> (symbol "#" >> return True)

pData :: Parser Machine
pData = do lights <- pLights
           buttons <- some $ delim "(" (someSepStr natural ",") ")"
           joltage <- delim "{" (someSepStr natural ",") "}"
           return (lights,buttons,joltage)

pInput :: Parser [Machine]
pInput = pLines pData

-- Part 1

toggle :: [Bool] -> [Int] -> [Bool]
toggle ls b = [if i `elem` b then not (ls!!i) else ls!!i | i <- [0..length ls - 1]]

pushAux :: [Bool] -> [[Int]] -> [Int]
pushAux ls bs = if and ls then [0] else concat $
  map (\b -> map (1+) $ pushAux (toggle ls b) (delete b bs)) bks
  where k = head $ filter (\j -> not (ls!!j)) [0..length ls - 1] -- first off light
        bks = filter (k `elem`) bs

push :: [Bool] -> [[Int]] -> [Int]
push ls bs = pushAux (map not ls) bs

part1 :: [Machine] -> Int
part1 = sum . map (\(ls,bs,_) -> minimum $ push ls bs) 

-- Part 2

type Jolt = M.Map [Int] Int

allJoltages :: [Int] -> [[Int]]
allJoltages [] = [[]]
allJoltages (j:js) = concat [map (x:) (allJoltages js) | x <- [0..j]]
  
{-
jolt :: [[Int]] -> [Int] -> Int
jolt buttons joltage = M.findWithDefault (-1) joltage joltMap where
  joltMap :: Jolt
  joltMap = M.fromList $
    (j0, 0) :
    [(j, 1 + minimum (1000000 : [M.findWithDefault 0 j' joltMap
                 | j' <- filter (all (>=0)) [jPushInv j b | b <- buttons]]))
     | j <- delete j0 $ allJoltages joltage]
  j0 =  map (const 0) joltage
-}

jPushInv :: [Int] -> [Int] -> [Int]
jPushInv b joltage = [if i `elem` b then (joltage!!i)-1 else joltage!!i |
                      i <- [0..length joltage-1]]

allPushes :: [Int] -> [Int] -> [(Int,[Int])]
allPushes b joltage = takeWhile (all (>=0)) $ iterate (jPushInv b) joltage

jolt :: [[Int]] -> [Int] -> Int
jolt bs joltage = if all (==0) joltage then 0 else case bs of
  [] -> sum joltage -- no solution
  (b:bs) -> minimum []

part2 :: [Machine] -> Int
part2 ms = sum $ map (\(_,bs,js) -> jolt bs js) ms

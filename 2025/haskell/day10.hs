-- Advent of Code 2025, day 10
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Data.Char
import Control.Applicative
import qualified Data.Map as M

import Numeric.LinearProgramming

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
      (noSol,sol) = partition (\(c,_,_) -> c==(-1)) $
                              map (\(_,bs,js) -> (jolt bs js, bs, js)) ms
      (_,bs,js) = head noSol
  putStrLn (show $ bs)
  putStrLn (show $ js)
  
  writeFile "machine.mod" (printModel bs js)
  -- sequence $ map (putStrLn . show) noSol
  -- putStrLn ("Part 1: " ++ show (part1 ms))
  putStrLn ("Part 2: " ++ show (sum [c | (c,_,_) <- sol]))

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

jPushInv :: [Int] -> [Int] -> [Int]
jPushInv b joltage = [if i `elem` b then (joltage!!i)-1 else joltage!!i |
                      i <- [0..length joltage-1]]

allPushes :: [Int] -> [Int] -> [(Int,[Int])]
allPushes b joltage =
  [0..] `zip` (takeWhile (all (>=0)) $ iterate (jPushInv b) joltage)

optProblem :: [[Int]] -> Optimization
optProblem buttons = Minimize (take (length buttons) (repeat 1))

constraints :: [[Int]] -> [Int] -> Constraints
constraints buttons joltage =
  Dense [[if i `elem` b then 1 else 0 | b <- bs] :==: (js!!i)
         | i <- [0..m-1]]
  where m = length joltage
        bs = map (map fromIntegral) buttons
        js = map fromIntegral joltage


jolt :: [[Int]] -> [Int] -> Int
jolt buttons joltage = if all isWhole psD then round solD else -1 -- intSolve prob constr where -- round solution where
  where  prob = optProblem buttons
         constr = constraints buttons joltage
         Optimal (solD,psD) = exact prob constr []
{-
         n = length buttons
         m = length joltage
         bs = map (map fromIntegral) buttons
         js = map fromIntegral joltage
         problem = Minimize (take n (repeat 1))
         constraints = Dense [[if i `elem` b then 1 else 0 | b <- bs] :==: (js!!i)
                             | i <- [0..m-1]]
  Optimal (solution,_) = exact problem constraints []
-}


joltP :: [[Int]] -> [Int] -> (Int,[Int])
joltP buttons joltage = (round solD, map round psD) where
  n = length buttons
  m = length joltage
  bs = map (map fromIntegral) buttons
  js = map fromIntegral joltage
  problem = Minimize (take n (repeat 1))
  constraints = Dense [[if i `elem` b then 1 else 0 | b <- bs] :==: (js!!i)
                      | i <- [0..m-1]]
  Optimal (solD,psD) = simplex problem constraints []


printModel :: [[Int]] -> [Int] -> String
printModel buttons joltage = vars ++ opt ++ constrs ++ res
  where nb = length buttons
        nj = length joltage
        vars = unlines $  map (\i -> "var b"++(show i)++" integer >=0;") [0..nb-1]
        vsum = intercalate " + " (map (('b':).show) [0..nb-1])
        constr j = "s.t. " ++
                   intercalate " + " ["b"++(show i) | i <- [0..nb-1], j `elem` buttons!!i]
                   ++ " = " ++ show (joltage!!j)++";"
        constrs = unlines $ map constr [0..nj-1]
        opt = "\nminimize obj: " ++ vsum ++ ";\n"
        res = "solve;\ndisplay "++ vsum ++";\nend;"
        

isWhole :: Double -> Bool
isWhole x = let n = floor x in x == fromIntegral n 

iNW :: [Double] -> (Int,Double)
iNW ds = (i,vi) where i = unJust $ findIndex (not.isWhole) ds
                      vi = ds!!i

intSolve :: Optimization -> Constraints -> Int
intSolve problem (Dense cs) =
  case simplex problem (Dense cs) [] of
     Optimal (solD,psD) -> if all isWhole psD then floor solD
       else let (i,vx) = iNW psD
                x = [if j==i then 1 else 0 | j <- [0..length psD - 1]]
                s0 = intSolve problem (Dense (x :<=: fromIntegral (floor vx) : cs))
            in if s0 == (floor solD) then s0 else
                min s0 (intSolve problem (Dense (x :>=: fromIntegral (ceiling vx) : cs)))
     _ -> 1000000

{-
  if all isWhole psD then floor solD else intSolve prob constr'
  where Optimal (solD,psD) = simplex prob constr []
        constr' = extendC constr
        extemdC (Dense cs) = Dense (prob :>=: solD+1 : cs)
-}

checkSol :: [[Int]] -> [Int] -> (Int,[Int]) -> Bool
checkSol buttons joltage (s,bs) =
  s == sum bs &&
  [ sum [b | (b,button) <- zip bs buttons, i `elem` button]
  | i <- [0..length joltage - 1]] == joltage

solJoltage :: [[Int]] -> Int -> [Int] -> [Int]
solJoltage buttons jn bs =
  [ sum [b | (b,button) <- zip bs buttons, i `elem` button]
  | i <- [0..jn - 1]]

checkAll :: [Machine] -> [Machine]
checkAll = filter mCheck where
  mCheck (_,bs,js) = not $ checkSol bs js (joltP bs js)

part2 :: [Machine] -> Int
part2 ms = sum $ map (\(_,bs,js) -> jolt bs js) ms

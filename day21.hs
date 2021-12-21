-- Advent of Code 2021, day 21

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
  putStrLn ("Part 1: " ++ show part1)
  putStrLn ("Part 2: " ++ show part2)

-- Parsing the input

pData :: Parser ()
pData = return ()

pInput :: Parser [()]
pInput = pLines pData

-- Part 1

type Pawn = (Int,Int)  -- position and score
type Die = (Int,Int)   -- times and value

score :: Pawn -> Int
score = snd

times :: Die -> Int
times = fst

roll :: Die -> (Int,Die)
roll (t,v) = (v+1, (t+1, (v+1) `mod` 100))

rolls :: Die -> (Int,Die)
rolls d = let (v1,d1) = roll d
              (v2,d2) = roll d1
              (v3,d3) = roll d2
          in (v1+v2+v3,d3)

posMv :: Int -> Int -> Int
posMv p v = (p+v-1) `mod` 10 + 1

move :: Die -> Pawn -> (Die,Pawn)
move d (p,s) = let (v,d') = rolls d
                   p' = posMv p v
                   s' = s+p'
               in (d',(p',s'))

play :: Die -> Pawn -> Pawn -> (Die,Pawn)
play d p1 p2 = if score p2 >= 1000 then (d,p1)
                 else let (d',p1') = move d p1 in play d' p2 p1'

{-
-- example
pawn1 = (4,0)
pawn2 = (8,0)
-}

pawn1 = (6,0)
pawn2 = (1,0)

part1 :: Int
part1 = let (d,p) = play (0,0) pawn1 pawn2
        in score p * times d

-- Part 2

type Dirac = M.Map (Pawn,Pawn) (Int,Int)

pawnMove :: Pawn -> Int -> Pawn
pawnMove (p,s) x = (posMv p x, s + posMv p x)

dRoll :: [Int]
dRoll = [1,2,3]

dRolls :: [Int]
dRolls = [ r1+r2+r3 | r1 <- dRoll, r2 <- dRoll, r3 <- dRoll ]


allPawns :: [Pawn]
allPawns = [ (p,s) | p <- [1..10], s <- [0..30] ]

dirac :: Dirac
dirac = M.fromList [( (p1,p2) , dScores p1 p2 )
                   | p1 <- allPawns, p2 <- allPawns]

dScores :: Pawn -> Pawn -> (Int,Int)
dScores p1 p2
  | score p2 >= 21 && score p1 <= score p2 = (0,1)
  | score p1 >= 21 = (1,0)
  | otherwise =
       let ws = map (\d -> dirac M.! (p2,(pawnMove p1 d))) dRolls
       in (sum (map snd ws), sum (map fst ws))

part2 :: Int
part2 = let (w1,w2) = dirac M.! (pawn1,pawn2) in max w1 w2

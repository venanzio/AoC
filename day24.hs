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

type Memory = (Int,Int,Int,Int)

getVar :: Var -> Memory -> Int
getVar W (w,x,y,z) = w
getVar X (w,x,y,z) = x
getVar Y (w,x,y,z) = y
getVar Z (w,x,y,z) = z

writeVar :: Var -> Int -> Memory -> Memory
writeVar W v (w,x,y,z) = (v,x,y,z) 
writeVar X v (w,x,y,z) = (w,v,y,z) 
writeVar Y v (w,x,y,z) = (w,x,v,z) 
writeVar Z v (w,x,y,z) = (w,x,y,v) 

eql x y = if x==y then 1 else 0

execOp :: Instruction -> Memory -> Memory
execOp (Add a (Left b)) m = writeVar a (getVar a m + getVar b m) m
execOp (Mul a (Left b)) m = writeVar a (getVar a m * getVar b m) m
execOp (Div a (Left b)) m = writeVar a (getVar a m `div` getVar b m) m
execOp (Mod a (Left b)) m = writeVar a (getVar a m `mod` getVar b m) m
execOp (Eql a (Left b)) m = writeVar a (getVar a m `eql` getVar b m) m
execOp (Add a (Right v)) m = writeVar a (getVar a m + v) m
execOp (Mul a (Right v)) m = writeVar a (getVar a m * v) m
execOp (Div a (Right v)) m = writeVar a (getVar a m `div` v) m
execOp (Mod a (Right v)) m = writeVar a (getVar a m `mod` v) m
execOp (Eql a (Right v)) m = writeVar a (getVar a m `eql` v) m
execOp i _ = error ("Can't execute instruction " ++ show i)

exec :: [Instruction] -> [Int] -> Memory -> Memory
exec [] _ m = m
exec (Inp a:is) (v:vs) m = exec is vs (writeVar a v m)
exec (i:is) vs m = exec is vs (execOp i m)

initMem :: Memory
initMem = (0,0,0,0)

allModels :: [[Int]]
allModels = amAux 14

amAux :: Int -> [[Int]]
amAux 1 = [[9],[8],[7],[6],[5],[4],[3],[2],[1]]
amAux n = [(d:ds) | d <- [9,8,7,6,5,4,3,2,1], ds <- amAux (n-1)]

search :: [Instruction] -> [[Int]] -> [Int]
search prog (mod:mods) =
  let m = exec prog mod initMem
  in if (getVar Z m) == 0
     then mod
     else search prog mods                     


spanInput :: [Instruction] -> ([Instruction],[Instruction])
spanInput (i:is) = let (p,ps) = span notInput is in (i:p,ps)
  where notInput (Inp _) = False
        notInput _ = True

-- Split program at input instructions
splitProg :: [Instruction] -> [[Instruction]]
splitProg [] = []
splitProg prog = let (p,prog') = spanInput prog
                 in p: splitProg prog'


nubMax :: (Ord a, Eq b) => [(a,b)] -> [(a,b)]
nubMax []= []
nubMax ((a,b):abs) =
  let (xs,ys) = spanBy (\ab -> snd ab == b) abs
      amax = maximum (a : map fst xs)
  in (amax,b):nubMax ys


execChunk :: [Instruction] -> Memory -> [(Int,Memory)]
execChunk prog m = map (\inp -> (inp, exec prog [inp] m)) [1..9]

execsChunk :: [Instruction] -> [([Int],Memory)] -> [([Int],Memory)]
execsChunk prog inpMems =
  nubMax $ concat $
    map (\(inp,m) -> map (\(v,m') -> (inp++[v],m'))
                         (execChunk prog m))
        inpMems

execAll :: [Instruction] -> [([Int],Memory)]
execAll prog = foldl (\inpMems p -> execsChunk p inpMems) [([],initMem)] (splitProg prog)

part1 :: [Instruction] -> [Int]
part1 prog = fst $ head $ filter (\(_,m) -> getVar Z m == 0) (execAll prog)


  

-- Part 2

part2 :: [Instruction] -> Int
part2 _ = 2

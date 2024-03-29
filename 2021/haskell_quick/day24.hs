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
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))


readProg :: IO [Instruction]
readProg = readFile "input" >>= return . parseAll pInput

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

memNorm :: Memory -> Memory
memNorm (w,x,y,z) = (w, x, y, z) 

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

initZ :: Int -> Memory
initZ z = (0,0,0,z)



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


selectChunks :: [[Instruction]] -> [[Instruction]]
selectChunks ps = [ps!!0,ps!!1,ps!!3,ps!!4,ps!!7,ps!!9,ps!!13]

select :: [a] -> [a]
select l = [l!!0,l!!1,l!!3,l!!4,l!!7,l!!9,l!!13]

pad :: [Int] -> [Int]
pad l = [l!!0,l!!1,9,l!!2,l!!3,9,9,l!!4,9,l!!5,9,9,9,l!!6]

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

decimal :: [Int] -> Int
decimal = foldl (\v d -> 10*v+d) 0


checkDigit :: Int -> Bool
checkDigit d = d>0 && d<10 

forwardF :: (Int,Int) -> (Int,Int) -> Int
forwardF (u,v) (z,w) =
  if z `mod` 26 == w-u
  then z+w+v
  else 26*z+w+v

backF :: (Int,Int) -> Int -> [(Int,Int)]
backF (u,v) zf = filter (\p -> forwardF (u,v) p == zf)
                   ([(zf-w-v,w) | w <- [1..9]] ++ [((zf-w-v) `div` 26,w) | w <- [1..9]])

{-
backF :: (Int,Int) -> Int -> [(Int,Int)]
backF (u,v) zf = bf1 ++ bf2 where
  bf1 = let z0 = zf-v+u `mod` 26
            w = z0 `div` 2
        in if checkDigit w && 2*w == z0 then [(zf-v-w,w)] else []
  bf2 = let z1 = zf-v
            w = z1 `mod` 26
            z2 = z1-w `div` 26
        in if checkDigit w && z2 `mod` 26 /= w-u then [(z2,w)] else []
-}

forwardT :: (Int,Int) -> (Int,Int) -> Int
forwardT (u,v) (z,w) = let z' = z `div` 26 in
  if z `mod` 26 + u == w
  then z'
  else 26*z'+w+v

backT :: (Int,Int) -> Int -> [(Int,Int)]
backT (u,v) zf = filter (\p -> forwardT (u,v) p == zf)
                   ([(26*zf+x,w) | w <- [1..9], x<- [0..25]] ++
                    [(26*((zf-w-v) `div` 26)+x,w) | w <- [1..9], x<- [0..25]])

progTable :: [(Int,Int,Bool)]
progTable = [(11,3,False),
             (14,7,False),
             (13,1,False),
             (-4,6,True),
             (11,14,False),
             (10,7,False),
             (-4,9,True),
             (-12,9,True),
             (10,6,False),
             (-11,4,True),
             (12,0,False),
             (-1,7,True),
             (0,12,True),
             (-11,1,True)]

forward1 :: (Int,Int,Bool) -> Int -> Int -> Int
forward1 (u,v,b) w z =
  if b then forwardT (u,v) (z,w)
       else forwardF (u,v) (z,w)

forward :: [(Int,Int,Bool)] -> [Int] -> Int -> Int
forward [] ws z = z
forward ((u,v,b):pT) (w:ws) z = forward pT ws (forward1 (u,v,b) w z)

back1 :: (Int,Int,Bool) -> Int -> [(Int,Int)]
back1 (u,v,b) zf = if b then backT (u,v) zf else backF (u,v) zf

back :: [(Int,Int,Bool)] -> Int -> [(Int,[Int])]
back [] zf = [(zf,[])]
back ((u,v,b):pT) zf =
  let zws = back pT zf
  in concat $ map (\(z,ws) -> map (\(z0,w) -> (z0,w:ws)) (back1 (u,v,b) z)) zws



sols :: [Int]
sols = sort $ map decimal $ map snd $ filter (\(z,_) -> z==0) $ back progTable 0









part1 :: [Instruction] -> Int
part1 _ = last sols
  -- decimal $ fst $ head $ filter (\(_,m) -> getVar Z m == 0) (execAll prog)


  

-- Part 2

part2 :: [Instruction] -> Int
part2 _ = head sols





{-
always read the input into w
  (w never otherwise modified)
then set x := z `mod` 26
  (so values of w and x at the beginning of a chunk not relevant)

the first time y is used:
         y := 25*x+1
    (so also value of y before a chunk not relevant)
  then multiply z by y and set y to 0 again

at the end of every section add y to z??



-}

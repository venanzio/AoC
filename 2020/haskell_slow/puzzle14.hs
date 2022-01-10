-- Advent of Code 2020, day 14

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

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

data Inst = UpdateMask String | WriteValue Int Int
  deriving (Eq,Show)

pMask :: Parser String
pMask = do symbol "mask"
           symbol "="
           some (char '0' <|> char '1' <|> char 'X')

pMem :: Parser (Int,Int)
pMem = do symbol "mem"
          symbol "["
          add <- natural
          symbol "]"
          symbol "="
          x <- natural
          return (add,x)

pInst :: Parser Inst
pInst = (pMask >>= \m     -> return $ UpdateMask m) <|>
        (pMem  >>= \(a,v) -> return $ WriteValue a v) 

pProg :: Parser [Inst]
pProg = some pInst

pInput = pProg

-- Part 1

data Mem = Mem { mask :: String
               , memory :: M.Map Int Int
               }
  deriving (Eq,Show)

type Binary = [Int]

decBin :: Int -> Binary
decBin n = dbAcc n [] where
  dbAcc 0 ds = ds
  dbAcc n ds = dbAcc (n `div` 2) (n `mod` 2:ds)

-- binary representation with 36 bits
bin36 :: Int -> Binary
bin36 n = let ds = decBin n in (take (36 - length ds) (repeat 0)) ++ ds

binDec :: Binary -> Int
binDec = foldl (\x d -> 2*x + d) 0

-- assume that the mask and the binary numbers have the same number of bits
applyMask :: String -> Binary -> Binary
applyMask "" [] = []
applyMask ('0':m) (d:x) = 0 : applyMask m x
applyMask ('1':m) (d:x) = 1 : applyMask m x
applyMask ('X':m) (d:x) = d : applyMask m x
applyMask _ _ = error "applyMask: mask doesn't match binary number"

appMask :: String -> Int -> Int
appMask m = binDec . applyMask m . bin36

exec :: Mem -> Inst -> Mem
exec mem (UpdateMask m) = mem {mask = m}
exec mem (WriteValue a v) =
  mem {memory = M.insert a (appMask (mask mem) v) (memory mem)}

initMem :: Mem
initMem = Mem { mask = replicate 36 'X',
                memory = M.empty }

execute1 :: [Inst] -> Mem
execute1 = foldl exec initMem

memValue :: Mem -> Int
memValue = M.foldr (+) 0 . memory

part1 :: [Inst] -> Int
part1 prog = memValue (execute1 prog)

-- Part 2

maskAddress :: String -> Binary -> [Binary]
maskAddress (x:m) (d:ds) = do
  bs <- maskAddress m ds
  b <- case x of
         '0' -> [d]
         '1' -> [1]
         'X' -> [0,1]
  return (b:bs)
maskAddress "" [] = return []
maskAddress _ _ = error "maskAddress: mask doesn't match binary number"

maskAddr :: String -> Int -> [Int]
maskAddr m =  map binDec . maskAddress m . bin36

insertAddrs :: M.Map Int v -> [Int] -> v -> M.Map Int v
insertAddrs m as v = foldl (\m a -> M.insert a v m) m as

exec2 :: Mem -> Inst -> Mem
exec2 mem (UpdateMask m) = mem {mask = m}
exec2 mem (WriteValue a v) =
  mem { memory = insertAddrs (memory mem) (maskAddr (mask mem) a) v}

execute2 :: [Inst] -> Mem
execute2 = foldl exec2 initMem

part2 :: [Inst] -> Int
part2 prog = memValue (execute2 prog)


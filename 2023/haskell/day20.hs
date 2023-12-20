-- Advent of Code 2023, day 20
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
  putStrLn (show $ configurationInit xs)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type ConjMemory = M.Map String Bool

data Module = Broadcaster | FlipFlop Bool | Conjunction ConjMemory
  deriving Show

type Configuration = M.Map String (Module,[String])

pBroadcaster :: Parser (String,Module)
pBroadcaster = symbol "broadcaster" >> return ("broadcaster",Broadcaster)

pFlipFlop :: Parser (String,Module)
pFlipFlop = symbol "%" >> word >>= \name -> return (name,FlipFlop False)

pConjunction :: Parser (String,Module)
pConjunction = symbol "&" >> word >>= \name -> return (name,Conjunction M.empty)

pModule :: Parser (String,(Module,[String]))
pModule = do (name,m) <- pBroadcaster <|> pFlipFlop <|> pConjunction 
             symbol "->"
             dest <- manySepStr word ","
             return (name,(m,dest))
    
pInput :: Parser Configuration
pInput = pLines pModule >>= return . M.fromList

-- Part 1

conjunctionInit :: Configuration -> String -> ConjMemory
conjunctionInit conf name =
  M.foldlWithKey (\ms mod (_,dest) -> if name `elem` dest
                   then M.insert mod False ms
                   else ms) M.empty conf

updateMem :: String -> (Module,[String]) -> ConjMemory -> (Module,[String])
updateMem name (Conjunction mem,dest) newmem = (Conjunction newmem,dest)
updateMem _ md _ = md

configurationInit :: Configuration -> Configuration
configurationInit conf = M.foldlWithKey initMod conf conf where
  initMod c name (mod) = M.insert name (updateMem name mod (conjunctionInit conf name)) c

-- High pulse = True, Low pulse = False

type Signal = (String,Bool,String)

sFrom :: Signal -> String
sFrom (f,_,_) = f

sTo :: Signal -> String
sTo (_,_,t) = t

sPulse :: Signal -> Bool
sPulse (_,p,_) = p

type Count = (Int,Int)

incCount :: Count -> Bool -> Count
incCount (lows,highs) b = if b then (lows,highs+1) else (lows+1,highs)

signal :: Count -> [Signal] -> Configuration -> (Count,Configuration)
signal count [] conf = (count,conf)
signal count ((from,pulse,to):sigs) conf = case M.lookup to conf of
  Nothing -> signal (incCount count pulse) sigs conf
  Just (FlipFlop b,dest) ->
    if pulse then signal (incCount count pulse) sigs conf
    else signal (incCount count pulse) (sigs++map (\d -> (to,not b,d)) dest)
                (M.insert to (FlipFlop (not b),dest) conf)
  Just (Conjunction mem,dest) ->
    let mem' = M.insert from pulse mem
        out = not $ and (M.elems mem')
    in signal (incCount count pulse) (sigs++map (\d -> (to,out,d)) dest)
              (M.insert to (Conjunction mem',dest) conf)
  Just (Broadcaster,dest) ->
    signal (incCount count pulse) (sigs++map (\d -> (to,pulse,d)) dest) conf


pushButton :: (Count,Configuration) -> (Count,Configuration)
pushButton (count,conf) = signal count [("button",False,"broadcaster")] conf



part1 :: Configuration -> Int
part1 conf = let (lows,highs) = fst $ nIter pushButton 1000 ((0,0),conf)
             in lows*highs 
                
-- Part 2

part2 :: Configuration -> Int
part2 _ = 2

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
      conf = configurationInit xs
  putStrLn ("Part 1: " ++ show (part1 conf))
  putStrLn ("Part 2: " ++ show (part2 conf))

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

sources :: Configuration -> String -> [String]
sources conf name = [mod | (mod, (_,dest)) <- M.toList conf, name `elem` dest]

lowMem :: [String] -> ConjMemory
lowMem = M.fromList . map (\s -> (s,False))

configurationInit :: Configuration -> Configuration
configurationInit conf = M.mapWithKey moduleInit conf where
  moduleInit name (Conjunction _, dest) = (Conjunction (lowMem $ sources conf name), dest)
  moduleInit _ mod = mod

  

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
  Nothing ->
    signal count sigs conf
  Just (FlipFlop b,dest) ->
    if pulse then signal count sigs conf
    else let b' = not b
             newsigs = map (\d -> (to,b',d)) dest
             count' = nIter (\c -> incCount c b') (length newsigs) count
         in signal count' (sigs++newsigs) (M.insert to (FlipFlop b',dest) conf)
  Just (Conjunction mem,dest) ->
    let mem' = M.insert from pulse mem
        out = not $ and (M.elems mem')
        newsigs = map (\d -> (to,out,d)) dest
        count' = nIter (\c -> incCount c out) (length newsigs) count
    in signal count' (sigs++newsigs) (M.insert to (Conjunction mem',dest) conf)
  Just (Broadcaster,dest) -> 
    let newsigs = map (\d -> (to,pulse,d)) dest
        count' = nIter (\c -> incCount c pulse) (length newsigs) count
    in signal count' (sigs++newsigs) conf


pushButton :: (Count,Configuration) -> (Count,Configuration)
pushButton (count,conf) = signal (incCount count False)
                                 [("button",False,"broadcaster")]
                                 conf

part1 :: Configuration -> Int
part1 conf = let (lows,highs) = fst $ nIter pushButton 1000 ((0,0),conf)
             in lows*highs
                
-- Part 2

signal2 :: [Signal] -> Configuration -> (Bool,Configuration)
signal2 [] conf = (False,conf)
signal2 ((_,False,"rx"):_) conf = (True,conf)
signal2 ((from,pulse,to):sigs) conf = case M.lookup to conf of
  Nothing ->
    signal2 sigs conf
  Just (FlipFlop b,dest) ->
    if pulse then signal2 sigs conf
    else let b' = not b
             newsigs = map (\d -> (to,b',d)) dest
          in signal2 (sigs++newsigs) (M.insert to (FlipFlop b',dest) conf)
  Just (Conjunction mem,dest) ->
    let mem' = M.insert from pulse mem
        out = not $ and (M.elems mem')
        newsigs = map (\d -> (to,out,d)) dest
    in signal2 (sigs++newsigs) (M.insert to (Conjunction mem',dest) conf)
  Just (Broadcaster,dest) -> 
    let newsigs = map (\d -> (to,pulse,d)) dest
    in signal2 (sigs++newsigs) conf

part2 :: Configuration -> Int
part2 conf = let (rxPulse,conf') = signal2 [("button",False,"broadcaster")] conf
             in if rxPulse then 1 else 1+part2 conf'

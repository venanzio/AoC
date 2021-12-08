-- Advent of Code 2021, day 8

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
  putStrLn (show (head xs))
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type Signal = [String]
type Output = [String]

pSignal :: Parser Signal
pSignal = some word

pOutput :: Parser Output
pOutput = some word

pData :: Parser (Signal,Output)
pData = do s <- pSignal
           string "|"
           o <- pOutput
           return (s,o)

pInput :: Parser [(Signal,Output)]
pInput = pLines pData

-- Part 1

count1478 :: Output -> Int
count1478 = length . filter (\x -> length x `elem` [2,4,3,7])

part1 :: [(Signal,Output)] -> Int
part1 =  sum . map (count1478 . snd)

-- Part 2

wiring :: [String]
wiring = ["abcefg","cf","acdeg","acdfg","bcdf","abdfg","abdefg","acf","abcdefg","abcdfg"]



-- candidate displays
type SegMap = [(Char,Char)]


segments :: Output -> SegMap
segments s =
  let [s1] = filter ((==2).length) s  -- signal for 1 (2 char)
      [s4] = filter ((==4).length) s  -- signal for 4
      [s7] = filter ((==3).length) s  -- signal for 7
      [s8] = filter ((==7).length) s  -- signal for 8

      ss069 = filter ((==6).length) s  -- signals for 0, 6, and 9
      ss235 = filter ((==5).length) s  -- signals for 2,3, and 5

      -- 'f' is in s1 and in 0, 6 and 9 
      [f] = filter (\c -> all (elem c) ss069) s1
      [c] = filter (/=f) s1

      -- 'a' is in 7 together with f and c
      [a] = s7 \\ [c,f]
             
      -- 6 is the only one of 0,6,9 to have f but not c
      [s6] = filter (\s -> elem f s && not (elem c s)) ss069

      -- 2 is the only one of 2,3,4 to have both c but not f, etc...
      [s2] = filter (\s -> elem c s && not (elem f s)) ss235
      [s3] = filter (\s -> elem c s && elem f s) ss235
      [s5] = filter (\s -> not (elem c s) && elem f s) ss235

      [d] = intersect (s4 \\ [c,f]) (s3 \\ [a,c,f])
      [b] = s4 \\ [c,d,f]
      [g] = s3 \\ [a,c,d,f]

      [e] = s6 \\ [a,b,d,f,g]
                 
  in [('a',a),('b',b),('c',c),('d',d),('e',e),('f',f),('g',g)]

lkup :: Eq a => a -> [(a,b)] -> b
lkup x l = let (Just y) = lookup x l in y

rewire :: SegMap -> String -> String
rewire sm s = map (\c -> lkup c sm) s

newWiring :: SegMap -> [String]
newWiring sm = map (rewire sm) wiring

seteq :: Eq a => [a] -> [a] -> Bool
seteq xs ys = (all (`elem` ys) xs) && (all (`elem` xs) ys)

decode :: [String] -> String -> Int
decode w s = let (Just n) = findIndex (seteq s) w in n

decimal :: [Int] -> Int
decimal = foldl (\x d -> 10*x + d) 0 

sigValue :: [String] -> Output -> Int
sigValue w o = decimal (map (decode w) o)

lineValue :: (Signal,Output) -> Int
lineValue (s,o) = let sm = segments s
                      w = newWiring sm
                  in sigValue w o

part2 :: [(Signal,Output)] -> Int
part2 = sum . map lineValue

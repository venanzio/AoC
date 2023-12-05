-- Advent of Code 2023, day 5
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
  let (sns,ms) = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 sns ms))
  putStrLn ("Part 2: " ++ show (part2 sns ms))

-- Parsing the input

type CMap = [(Int,Int,Int)]

pData :: Parser ()
pData = return ()



pInput :: Parser ([Int],[CMap])
pInput = do symbol "seeds:"
            sns <- many natural
            symbol "seed-to-soil map:"
            s2s <- many (pTriple natural natural natural)
            symbol "soil-to-fertilizer map:"
            s2f <- many (pTriple natural natural natural)
            symbol "fertilizer-to-water map:"
            f2w <- many (pTriple natural natural natural)
            symbol "water-to-light map:"
            w2l <- many (pTriple natural natural natural)
            symbol "light-to-temperature map:"
            l2t <- many (pTriple natural natural natural)
            symbol "temperature-to-humidity map:"
            t2h <- many (pTriple natural natural natural)
            symbol "humidity-to-location map:"
            h2l <- many (pTriple natural natural natural)
            return (sns,[s2s,s2f,f2w,w2l,l2t,t2h,h2l])

            
-- Part 1

fMap :: CMap -> Int -> Int
fMap [] x = x
fMap ((d,s,r):ms) x = if s <= x && x < s+r then d+x-s else fMap ms x

lCompose :: [a -> a] -> a -> a
lCompose [] x = x
lCompose (f:fs) x = lCompose fs (f x)


part1 :: [Int] -> [CMap] -> Int
part1 sns ms = let fs = map fMap ms
                   f = lCompose fs
               in minimum (map f sns)

-- Part 2

interSplit :: Range -> Range -> (Ranges,Ranges)
interSplit r0 r1 =
  let mr = rIntersect r0 r1
  in if rIsEmpty mr then ([],[r0])
                    else ([mr], rDiff r0 mr)

iSplit :: Ranges -> Range -> (Ranges,Ranges)
iSplit [] r1 = ([],[])
iSplit (r0:rs0) r1 =
  let (i0,i1) = interSplit r0 r1
      (is0,is1) = iSplit rs0 r1
  in (i0++is0, i1++is1)


rangeMap :: (Int,Int,Int) -> Int -> Int
rangeMap (d,s,r) x = d+x-s

rMap :: CMap -> Ranges -> Ranges
rMap [] rs = rs
rMap ((d,s,l):ms) rs =
  let (mrs,irs) = iSplit rs (rangeL s l)
  in map (\(x,y) -> (rangeMap (d,s,l) x, rangeMap (d,s,l) y)) mrs ++
     rMap ms irs

seedRanges :: [Int] -> Ranges
seedRanges [] = []
seedRanges (s:l:sls) = rangeL s l : seedRanges sls

rMinimum :: Ranges -> Int
rMinimum = minimum . map fst . neRanges

part2 :: [Int] -> [CMap]-> Int
part2 sns ms = 
  let fs = map rMap ms
      f = lCompose fs
  in rMinimum (f (seedRanges sns))


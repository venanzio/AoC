-- Advent of Code 2023, day 5
--  Venanzio Capretta

module Main where

import System.Environment
import Control.Applicative
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

rangeMap :: (Int,Int,Int) -> Int -> Int
rangeMap (d,s,r) x = d+x-s

fMap :: CMap -> Int -> Int
fMap [] x = x
fMap ((d,s,r):ms) x = if s <= x && x < s+r
                      then rangeMap (d,s,r) x
                      else fMap ms x

part1 :: [Int] -> [CMap] -> Int
part1 sns ms = let fs = map fMap ms
                   f = fsCompose fs
               in minimum (map f sns)

-- Part 2

-- split an interval between the intersection with another range and the rest
interSplit :: Interval -> Interval -> (Range,Range)
interSplit r0 r1 = let mr = iIntersect r0 r1 in ([mr], rDiff r0 mr)

iSplit :: Range -> Interval -> (Range,Range)
iSplit [] r1 = ([],[])
iSplit (r0:rs0) r1 =
  let (i0,i1) = interSplit r0 r1
      (is0,is1) = iSplit rs0 r1
  in (i0++is0, i1++is1)

rMap :: CMap -> Range -> Range
rMap [] rs = rs
rMap ((d,s,l):ms) rs =
  let (mrs,irs) = iSplit rs (rangeL s l)
  in intsRange (map (\(x,y) -> (rangeMap (d,s,l) x, rangeMap (d,s,l) y)) mrs
                ++ rMap ms irs)

seedRange :: [Int] -> Range
seedRange [] = []
seedRange (s:l:sls) = rangeL s l : seedRange sls

part2 :: [Int] -> [CMap]-> Int
part2 sns ms = 
  let fs = map rMap ms
      f = fsCompose fs
  in rMinimum (f (seedRange sns))


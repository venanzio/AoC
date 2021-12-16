-- Advent of Code 2021, day 16

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
  let p = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 p))
  -- putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

data Packet = Literal Int Int | Operator Int Int [Packet]
  deriving (Eq,Show)

version :: Packet -> Int
version (Literal v _) = v
version (Operator v _ _) = v

type Bit = Int

hexBinary :: Char -> [Bit]
hexBinary '0' = [0,0,0,0]
hexBinary '1' = [0,0,0,1]
hexBinary '2' = [0,0,1,0]
hexBinary '3' = [0,0,1,1]
hexBinary '4' = [0,1,0,0]
hexBinary '5' = [0,1,0,1]
hexBinary '6' = [0,1,1,0]
hexBinary '7' = [0,1,1,1]
hexBinary '8' = [1,0,0,0]
hexBinary '9' = [1,0,0,1]
hexBinary 'A' = [1,0,1,0]
hexBinary 'B' = [1,0,1,1]
hexBinary 'C' = [1,1,0,0]
hexBinary 'D' = [1,1,0,1]
hexBinary 'E' = [1,1,1,0]
hexBinary 'F' = [1,1,1,1]


pHex :: Parser [Bit]
pHex = do hex <- many alphanum
          return (concat $ map hexBinary hex)

pInput :: Parser Packet
pInput = pHex >>= return . packet

-- Decoding the packets

binDec :: [Bit] -> Int
binDec = foldl (\x b -> 2*x + b) 0

type Source = (Int,[Bit]) -- first Int is read charaters mod 4

step :: Int -> Int -> Int
step m n = (m+n) `mod` 4

readBits :: Int -> Source -> ([Bit],Source)
readBits n (m,bits) = let (x,bits') = splitAt n bits
                      in (x, (step m n,bits'))

readVersion :: Source -> (Int,Source)
readVersion s = let (bs,s1) = readBits 3 s
                in (binDec bs,s1)

readId = readVersion

readLit :: [Bit] -> Source -> ([Bit],Source)
readLit bs s0 = let (cs,s1) = readBits 5 s0
                    bs' = bs ++ tail cs
                in if head cs == 0 then (bs',s1)
                                   else readLit bs' s1

readLiteral :: Source -> (Int, Source)
readLiteral s = let (bs,s1) = readLit [] s
                in (binDec bs, s1)

readTL :: Source -> (Bit, Source)
readTL s = let ([tl],s1) = readBits 1 s
           in (tl,s1)

readSubLength :: Source -> ([Packet],Source)
readSubLength s = let (bs,s1) = readBits 15 s
                      len = binDec bs
                      (bs',s2@(m,_)) = readBits len s1
                      (ps,_) = readPackets (m,bs')
                  in (ps,s2)

readSN :: Int -> Source -> ([Packet],Source)
readSN 0 s = ([],s)
readSN n s = let (p,s1) = readPacket s
                 (ps,s2) = readSN (n-1) s1
             in (p:ps,s2)

readSubNumber :: Source -> ([Packet],Source)
readSubNumber s = let (bs,s1) = readBits 11 s
                      num = binDec bs
                  in readSN num s1

readSubPackets :: Source -> ([Packet],Source)
readSubPackets s = let (tl,s1) = readTL s
                    in if tl == 0 then readSubLength s1
                                  else readSubNumber s1

packetEnd :: Source -> Source
packetEnd s = s
{-
packetEnd (0,bs) = (0,bs)
packetEnd (m,bs) = (0,drop (4-m) bs)
-}

readPacket :: Source -> (Packet,Source)
readPacket s = let (v,s1) = readVersion s
                   (i,s2) = readId s1
               in if i == 4 then let (x,s3) = readLiteral s2
                                     s4 = packetEnd s3
                                 in (Literal v x, s4)
                            else let (ps,s3) = readSubPackets s2
                                     s4 = packetEnd s3
                                 in (Operator v i ps, s4)

readPackets :: Source -> ([Packet],Source)
readPackets (m,[]) = ([],(m,[]))
readPackets s = let (p,s1) = readPacket s
                    (ps,s2) = readPackets s1
                in (p : ps, s2)


packets :: [Bit] -> [Packet]
packets bs = fst $ readPackets (0,bs)

packet :: [Bit] -> Packet
packet bs = fst $ readPacket (0,bs)

  
-- Part 1

sumVersion :: Packet -> Int
sumVersion (Literal v _) = v
sumVersion (Operator v _ ps) = v + sum (map sumVersion ps)

part1 :: Packet -> Int
part1 = sumVersion

-- Part 2

part2 :: Packet -> Int
part2 _ = 2

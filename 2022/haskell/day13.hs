-- Advent of Code 2022, day 13
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
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

data Packet = PValue Int | PList [Packet]
  deriving (Show,Eq)

pPacket :: Parser Packet
pPacket = do n <- natural
             return (PValue n)
          <|>
          do ps <- delim "[" (manySepStr pPacket ",") "]"
             return (PList ps)

pInput :: Parser [(Packet,Packet)]
pInput = many (do p1 <- pPacket
                  p2 <- pPacket
                  return (p1,p2))

-- Part 1

instance Ord Packet where
  -- compare :: Packet -> Packet -> Ordering
  compare (PValue n1)   (PValue n2)   = compare n1 n2
  compare (PList ps1)   (PList ps2)   = compare ps1 ps2
  compare p1@(PValue _) p2            = compare (PList [p1]) p2
  compare p1            p2@(PValue _) = compare p1 (PList [p2])

part1 :: [(Packet,Packet)] -> Int
part1 ps = sum $ map ((+1).fst) $ filterIndices (\(p1,p2) -> p1<=p2) ps

-- Part 2

divP1 :: Packet 
divP1 = PList [PList [PValue 2]]

divP2 :: Packet 
divP2 = PList [PList [PValue 6]]
  
flatten :: [(Packet,Packet)] -> [Packet]
flatten [] = []
flatten ((p1,p2):ps) = p1 : p2 : flatten ps

decoderKey :: [Packet] -> Int
decoderKey ps =
  let Just i1 = elemIndex divP1 ps
      Just i2 = elemIndex divP2 ps
  in (i1+1) * (i2+1)

part2 :: [(Packet,Packet)] -> Int
part2 = decoderKey . sort . ([divP1,divP2]++) . flatten

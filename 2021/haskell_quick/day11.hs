-- Advent of Code 2021, day 11

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
  let o = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 o))
  putStrLn ("Part 2: " ++ show (part2 o))

-- Parsing the input

type Octo = M.Map (Int,Int) Int

pData :: Parser [Int]
pData = some (digit >>= \c -> return (read [c]))

pInput :: Parser Octo
pInput = pLines pData >>= return . mMap

-- Part 1

incOcto :: Octo -> Octo
incOcto = M.map (+1)

flash :: Octo -> [(Int,Int)] -> Octo
flash o cs = foldl (\o c -> M.update incr c o) o cs
  where incr x = if x == 0 then Just 0 else Just (x+1)

adj :: (Int,Int) -> [(Int,Int)]
adj (i,j) = [(i',j') | i' <- [i-1 .. i+1], i'>=0, i'<=9,
                       j' <- [j-1 .. j+1], j'>=0, j'<=9, (i',j') /= (i,j)] 

stepOne :: Octo -> (Int,Int) -> Maybe Octo
stepOne o c = if o M.!c > 9 then Just (M.update (\_ -> Just 0) c $ flash o (adj c))
                            else Nothing

coordinates = [(i,j) | i <- [0..9], j<- [0..9]]

step' :: Octo -> (Octo,Int)
step' o = foldl (\(o',count) c -> case stepOne o' c of
                    Nothing -> (o',count)
                    Just o'' -> (o'',count+1))
                (o,0) coordinates

step :: (Octo,Int) -> (Octo,Int)
step (o,count) =
  let (o',count') = step' o
  in  if count' == 0 then (o',count)
                     else step (o',count+count')

        
steps :: (Octo,Int) -> Int -> (Octo,Int)
steps (o,count) 0 = (o,count) 
steps (o,count) n = let (o',count') = step (incOcto o,count)
                    in steps (o',count') (n-1)

part1 :: Octo -> Int
part1 o = snd $ steps (o,0) 100

-- Part 2

allFlash :: Octo -> Bool
allFlash = all (==0) . M.elems

flashTime :: Octo -> Int
flashTime o = if allFlash o then 0
                else let (o',_) = step (incOcto o,0)
                     in (flashTime o') + 1

part2 :: Octo -> Int
part2 = flashTime

-- Advent of Code 2025, day 7
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Data.Char
import Data.Maybe
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
      (w,h,m) = xs
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser [String]
pData = some line

pInput :: Parser (Int, Int, Map2D Char)
pInput = do ls <- some line
            let width = length (ls!!1)
                height = length ls
                manifold = mMapF (\c -> if c=='.' then Nothing else Just c) ls
            return (width, height, manifold)

-- Part 1

toSplitter :: Int -> Map2D Char -> Point -> (Int, Map2D Char)
toSplitter height manifold (x,y)
  | y == height || nextP == Just '|' = (0, manifold)
  | nextP == Nothing = toSplitter height (M.insert (x,y) '|' manifold) (x,y+1)
  | nextP == Just '^' = (1+nL+nR, splitR)
  | otherwise = error (show nextP ++ show (x,y))
  where nextP = M.lookup (x,y) manifold
        (nL, splitL) = toSplitter height manifold (x-1,y)
        (nR, splitR) = toSplitter height splitL (x+1,y)

part1 :: (Int, Int, Map2D Char) -> Int
part1 (_, height, manifold) = fst $ toSplitter height manifold (xS,yS+1)
  where (xS,yS) = head $ mFind 'S' manifold

-- Part 2

type Timelines = Map2D Int

qSplitter :: Point -> Map2D Char -> Int
qSplitter (width,height) manifold = fromJust $ M.lookup pS timelines where
  pS = head $ mFind 'S' manifold
  timelines = pointMap (0,0) (width+1,height) qSplit
  qSplit (x,y) 
    | y == height = 1
    | pChar == Nothing || pChar == Just 'S' = fromJust (M.lookup (x,y+1) timelines)
    | pChar == Just '^' = fromJust (M.lookup (x-1,y) timelines) +
                          fromJust (M.lookup (x+1,y) timelines)
    where pChar = M.lookup (x,y) manifold

part2 :: (Int, Int, Map2D Char) -> Int
part2 (width, height, manifold) = qSplitter (width,height) manifold

-- Advent of Code 2022, day 14
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

data Point = Rock | Sand
  deriving (Eq,Show)
type Coords = (Int,Int)
type Cave = M.Map Coords Point

pRockPath :: Parser [Coords]
pRockPath = do x <- natural
               symbol ","
               y <- natural
               (do symbol "->"
                   p <- pRockPath
                   return ((x,y):p))
                <|> return [(x,y)]

pathLine :: Coords -> Coords -> [Coords]
pathLine (x1,y1) (x2,y2)
  | x1==x2 = [(x1,y) | y <- [min y1 y2 .. max y1 y2]]
  | y1==y2 = [(x,y1) | x <- [min x1 x2 .. max x1 x2]]
  | otherwise = error "no straight path"

fullPath :: [Coords] -> [Coords]
fullPath (c1:c2:cs) = pathLine c1 c2 ++ fullPath (c2:cs)
fullPath [c] = [c]

rockCave :: [Coords] -> Cave
rockCave cs = M.fromList $ zip cs (repeat Rock)

pInput :: Parser Cave
pInput = do ps <- pLines pRockPath
            let rs = concat $ map fullPath ps
            return (rockCave rs)

-- Part 1

blocked :: Coords -> Cave -> Bool
blocked c cave = M.lookup c cave `elem` [Just Rock, Just Sand]

fallStep :: Coords -> Cave -> Maybe Coords
fallStep (x,y) cave
  | not (blocked (x,y+1) cave) = Just (x,y+1)
  | not (blocked (x-1,y+1) cave) = Just (x-1,y+1)
  | not (blocked (x+1,y+1) cave) = Just (x+1,y+1)
  | otherwise = Nothing

fallSand :: Coords -> Int -> Cave -> Maybe Cave
fallSand c n cave
  | snd c > n = Nothing
  | otherwise = case fallStep c cave of
      Nothing -> Just (M.insert c Sand cave)
      Just c' -> fallSand c' n cave

countSand :: Int -> Cave -> Int
countSand n cave = case M.lookup (500,0) cave of
  Just Sand -> 0
  _ -> case fallSand (500,0) n cave of
    Nothing -> 0
    Just cave' -> countSand n cave' + 1

bottom :: Cave -> Int
bottom = maximum . map snd . M.keys

part1 :: Cave -> Int
part1 cave = countSand (bottom cave) cave

-- Part 2

minX :: Cave -> Int
minX = minimum . map fst . M.keys

maxX :: Cave -> Int
maxX = maximum . map fst . M.keys

rockFloor :: Int -> Cave
rockFloor n = rockCave [(x,n) | x <- [500-n .. 500+n]]

addFloor :: Cave -> Cave
addFloor cave = M.union cave (rockFloor (bottom cave + 2))
  
part2 :: Cave -> Int
part2 cave = countSand (bottom cave + 2) (addFloor cave)

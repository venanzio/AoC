-- Advent of Code 2024, day 18
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
-- import Data.Char
-- import Control.Applicative
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
      graph = spaceGraph (take drops xs)
      path = head $ snd $ dijkstraPaths graph (0,0) (maxX,maxY) -- findPath graph (0,0) (maxX,maxY)
  putStrLn (showManyPoints [('#',take drops xs),('O',path)])
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser Point
pData = do x <- natural
           symbol ","
           y <- natural
           return (x,y)

pInput :: Parser [Point]
pInput = pLines pData

-- Part 1

maxX = 70
maxY = 70

drops = 1024

spaceGraph :: [Point] -> Graph Point
spaceGraph corrupt =
  M.fromList [((i,j), [(m,1) | m <- neighboursHV (i,j), not (m `elem` corrupt)]) |
                           i <- [0..maxX], j <- [0..maxY]]

part1 :: [Point] -> Int
part1 xs = dijkstra (spaceGraph (take drops xs)) (0,0) (maxX,maxY)

-- Part 2

{- Idea: chose a path (initially directly diagonally)
         revise when one of the bytes fall on it -}

exitPath :: Graph Point -> Maybe [Point]
exitPath graph = findPath graph (0,0) (maxX,maxY)


part2 :: [Point] -> Int
part2 _ = 2

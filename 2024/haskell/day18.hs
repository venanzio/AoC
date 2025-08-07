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
      (pre,post) = splitAt drops xs
      graph = spaceGraph pre
      (len,paths) = dijkstraPaths graph (0,0) (maxX,maxY)
  putStrLn (showManyPoints [('#',pre),('O',head paths)])
  putStrLn ("Part 1: " ++ show len)
  putStrLn ("Part 2: " ++ show (cutOff paths pre post))

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

{- Idea: chose some path (initially that from part 1)
         then recompute when they're all blocked -}

-- breaking at the point where all paths are blocked
allBlocked :: [[Point]] -> [Point] -> ([Point],[Point])
allBlocked paths []     = ([],[])
allBlocked paths (p:ps) =
  let paths' = [path | path <- paths, not (p `elem` path)]
      (pre,post) = allBlocked paths' ps
  in if paths' == [] then ([],p:ps) else (p:pre,post)


cutOff :: [[Point]] -> [Point] -> [Point] -> Point
cutOff paths qs ps =
  if paths == [] then p else cutOff paths (p:pre++qs) post
  where (pre,p:post) = allBlocked paths ps
        paths = snd $ dijkstraPaths (spaceGraph (p:pre)) (0,0) (maxX,maxY)

part2 :: [Point] -> Int
part2 _ = 2

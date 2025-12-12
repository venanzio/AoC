-- Advent of Code 2025, day 12
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
  let (shs,regs) = parseAll pInput input
  putStrLn ("Part 1")
  putStrLn ("  possible : " ++ show (length regs - tooHard shs regs))
  putStrLn ("  very easy: " ++ show (easy shs regs))


-- Parsing the input

type Shape = [Point]

pShape :: Parser Shape
pShape = do natural >> symbol ":"
            sh <- some neLine
            return [(x,y) | y <- [0..length sh -1], x <- [0..length (sh!!y) -1],
                            (sh!!y)!!x == '#']

pRegion :: Parser (Int,Int,[Int])
pRegion = do width <- natural
             symbol "x"
             height <- natural
             symbol ":"
             shapes <- pLine (some natural)
             return (width,height,shapes)


pInput :: Parser ([Shape],[(Int,Int,[Int])])
pInput = do shapes <- some pShape
            regions <- some pRegion
            return (shapes,regions)

-- Part 1

adjust :: Shape -> Shape
adjust sh = map (\(x,y) -> (x-minX,y-minY)) sh
  where minX = minimum (map pX sh)
        minY = minimum (map pY sh)

flipX :: Shape -> Shape
flipX = adjust . map (\(x,y) -> (-x,y))

flipY :: Shape -> Shape
flipY = adjust . map (\(x,y) -> (x,-y))

rotate :: Shape -> Shape
rotate = map (\(x,y) -> (y,x))

flipRot :: Shape -> [Shape]
flipRot sh = nub (flips ++ (map rotate flips))
    where flips = [sh, flipX sh, flipY sh, flipX $ flipY sh]


minArea :: [Shape] -> [Int] -> Int
minArea shs ns = sum [(length (shs!!i))*(ns!!i) | i <- [0..length shs -1]]

maxArea ::  [Int] -> Int
maxArea ns = 9 * sum ns

easy :: [Shape] -> [(Int,Int,[Int])] -> Int
easy shs regs = length $ filter (\(w,h,ns) -> maxArea ns <= w*h) regs

tooHard :: [Shape] -> [(Int,Int,[Int])] -> Int
tooHard shs regs = length $ filter (\(w,h,ns) -> minArea shs ns > w*h) regs


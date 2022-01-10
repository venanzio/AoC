-- Advent of Code 2021, day 20

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
  let (enAlg,image) = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 enAlg image))
  putStrLn ("Part 2: " ++ show (part2 enAlg image))

-- Parsing the input

type EnAlg = [Int]
type Image = [[Int]]

pPixel :: Parser Int
pPixel = (char '.' >> return 0) <|> (char '#' >> return 1)

pImage :: Parser Image
pImage = some (token $ some pPixel)

pInput :: Parser (EnAlg,Image)
pInput = do enAlg <- many pPixel
            image <- pImage
            return (enAlg,image)

-- Part 1

-- add a frame of 0s
frame :: Image -> Image
frame p = let emptyRow = take (length (head p) + 2) (repeat 0)
          in emptyRow : map (\row -> 0:row ++ [0]) p ++ [emptyRow]

frameN :: Int -> Image -> Image
frameN = nIter frame

square :: Image -> Int -> Int -> [Int]
square image i j = [image!!h!!k | h <- [i-1..i+1], k <- [j-1..j+1]]

binary :: [Int] -> Int
binary = foldl (\v d -> 2*v+d) 0

pixMap :: EnAlg -> [Int] -> Int
pixMap enAlg sq = enAlg!!(binary sq)

imageEnhance :: EnAlg -> Image -> Image
imageEnhance enAlg img =
  let hgt = length img
      wdt = length (head img)
  in [ [pixMap enAlg (square img i j) | j <- [1..wdt-2]] | i <- [1..hgt-2]] 

countLit :: Image -> Int
countLit = sum . map sum

part1 :: [Int] -> Image -> Int
part1 enAlg img = countLit $ nIter (imageEnhance enAlg) 2 $ frameN 4 img

-- Part 2

part2 :: [Int] -> Image -> Int
part2 enAlg img = countLit $ nIter (imageEnhance enAlg) 50 $ frameN 100 img


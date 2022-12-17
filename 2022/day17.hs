-- Advent of Code 2022, day 17
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
  putStrLn (show rocks)
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type Queue a = ([a],[a])

queue :: [a] -> Queue a
queue xs = (xs,[])

nextQ :: Queue a -> (a,Queue a)
nextQ (x:xs,ys) = (x,(xs,x:ys))
nextQ ([],ys) = nextQ (reverse ys,[])

pInput :: Parser (Queue Char)
pInput = many (char '>' <|> char '<') >>= return . queue

type Rock = [(Int,Int)]

rockL :: String -> [Int]
rockL s = map fst $ filter (\(_,c) -> c=='#') (zip [0..] s)

rockS :: [String] -> Rock
rockS ss = concat $
            map (\(y,l) -> map (\x->(x,y)) l) $
             zip [0..] (reverse (map rockL ss))

rocksS :: [String] -> [Rock]
rocksS ss =  map rockS $ divide "" ss

rocks :: Queue Rock
rocks = queue $ rocksS $
  lines "####\n\n.#.\n###\n.#.\n\n..#\n..#\n###\n\n#\n#\n#\n#\n\n##\n##"

-- Part 1

shiftRock :: (Int,Int) -> Rock -> Rock
shiftRock (x,y) = map (\(x0,y0) -> (x0+x,y0+y))

mvDown = shiftRock (0,-1)
mvLeft = shiftRock (-1,0)
mvRight = shiftRock (0,1)

leftWContact :: Rock -> Bool
leftWContact = any (\(x,_) -> x==0)

rightWContact :: Rock -> Bool
rightWContact = any (\(x,_) -> x==6)

hitBottom :: Rock -> Bool
hitBottom = any (\(_,y) -> y==1)

rockContact :: Rock -> Rock -> Bool
rockContact r0 r1 = any touch r0 where
  touch (x,y) = any (\(x0,y0) -> x0==x && y0==x+1) r1

towerHeight :: Rock -> Int
towerHeight = maximumB 0 . map snd

rockFall :: Rock -> Queue Char -> Rock -> (Queue Char,Rock)
rockFall r wind tower =
  let (gust,wind') = nextQ wind
      r1 = case gust of
             '<' -> if leftWContact r then r else mvLeft r
             '>' -> if rightWContact r then r else mvRight r
      r2 = mvDown r1
  in if rockContact r1 tower
       then (wind', r1 ++ tower)
       else if rockContact r2 tower || hitBottom r2
              then (wind', r2 ++ tower)
              else rockFall r2 wind' tower
  

fallingRocks :: Queue Rock -> Queue Char -> Rock -> Int -> Int
fallingRocks _ _ tower 0 = error (show tower) -- towerHeight tower
fallingRocks rs wind tower n =
  let (r,rs') = nextQ rs
      h = towerHeight tower
      r0 = shiftRock (2,h+3) r
      (wind',tower') = rockFall r0 wind tower
  in fallingRocks rs' wind' tower' (n-1)
      
part1 :: Queue Char -> Int
part1 wind = fallingRocks rocks wind [] 2022

-- Part 2

part2 :: Queue Char -> Int
part2 _ = 2

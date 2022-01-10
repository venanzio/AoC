-- Advent of Code 2021, day 25

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
  -- putStrLn ("Part 2: " ++ show (part2 xs))

readF :: IO Floor
readF = readFile "exinput" >>= return . parseAll pInput

showF :: Floor -> String
showF floor = sF 0 0 where
  sF x y = case M.lookup (x,y) floor of
    Just c -> c : sF (x+1) y
    Nothing -> '\n' : if M.lookup (0,y+1) floor == Nothing
                      then ""
                      else sF 0 (y+1)                    

-- Parsing the input

type Floor = M.Map (Int,Int) Char

pRow :: Parser [Char]
pRow =  many (char '.' <|> char '>' <|> char 'v')

pInput :: Parser Floor
pInput = pLines pRow >>= return . mMap 

-- Part 1

nextR :: Floor -> (Int,Int) -> (Int,Int)
nextR floor (x,y) = case M.lookup (x,y) floor of
  Just '>' -> case M.lookup (x+1,y) floor of
           Nothing -> case M.lookup (0,y) floor of
             Just '.' -> (0,y)
             _        -> (x,y)
           Just '.' -> (x+1,y)
           _        -> (x,y)
  _        -> (x,y)

mobileR :: Floor -> [((Int,Int),(Int,Int))]
mobileR floor = filter (\((x,y),(newx,newy)) -> x/=newx || y/=newy)
                       (map (\p -> (p,nextR floor p)) (M.keys floor))

moveR :: Floor -> Maybe Floor
moveR floor = case mobileR floor of
  [] -> Nothing
  moves -> Just $
           foldl (\f ((x,y),(newx,newy)) -> M.insert (newx,newy) '>' $ M.insert (x,y) '.' f)
                 floor moves


nextD :: Floor -> (Int,Int) -> (Int,Int)
nextD floor (x,y) = case M.lookup (x,y) floor of
  Just 'v' -> case M.lookup (x,y+1) floor of
           Nothing -> case M.lookup (x,0) floor of
             Just '.' -> (x,0)
             _        -> (x,y)
           Just '.' -> (x,y+1)
           _        -> (x,y)
  _        -> (x,y)

mobileD :: Floor -> [((Int,Int),(Int,Int))]
mobileD floor = filter (\((x,y),(newx,newy)) -> x/=newx || y/=newy)
                       (map (\p -> (p,nextD floor p)) (M.keys floor))

moveD :: Floor -> Maybe Floor
moveD floor = case mobileD floor of
  [] -> Nothing
  moves -> Just $
           foldl (\f ((x,y),(newx,newy)) -> M.insert (newx,newy) 'v' $ M.insert (x,y) '.' f)
                 floor moves

move :: Floor -> Maybe Floor
move floor =
  let f0 = moveR floor
      floor0 = case f0 of
                 Nothing -> floor
                 Just f -> f
      f1 = moveD floor0
      floor1 = case f1 of
                 Nothing -> floor0
                 Just f -> f
  in if f0 == Nothing && f1 == Nothing then Nothing else Just floor1
      
moveCount :: Floor -> Int
moveCount floor = case move floor of
  Nothing -> 1
  Just floor' -> 1 + moveCount floor'

part1 :: Floor -> Int
part1 = moveCount 

-- Part 2

part2 :: Floor -> Int
part2 _ = 2

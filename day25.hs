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
  putStrLn (show $ xs M.! (8,17))
  -- putStrLn ("Part 1: " ++ show (part1 xs))
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

part1 :: Floor -> Int
part1 _ = 1

-- Part 2

part2 :: Floor -> Int
part2 _ = 2

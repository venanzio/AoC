-- Advent of Code 2024, day 15
--  Venanzio Capretta

module Main where

import System.Environment
-- import Data.List
-- import Data.Char
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
  let (h,ms) = parseAll pInput input
  -- stepMoves (robot h) ms h
  putStrLn ("Part 1: " ++ show (part1 h ms))
  putStrLn ("Part 2: " ++ show (part2 h ms))

-- Parsing the input

pMapLine :: Parser String
pMapLine = do char '#'
              s <- chars
              return ('#':s)

pHouse :: Parser (Map2D Char)
pHouse = do some pMapLine >>= return.stringsMap
             
pMoves :: Parser String
pMoves = some (choice (map (token.char) "^v<>"))

pInput :: Parser (Map2D Char, String)
pInput = do h <- pHouse
            ms <- pMoves
            return (h,ms)

-- Part 1

robot :: Map2D Char -> Point
robot = head.(mFind '@')

mDir :: Char -> Direction
mDir '^' = dUp
mDir 'v' = dDown
mDir '<' = dLeft
mDir '>' = dRight

moveH :: Point -> Direction -> Map2D Char -> Maybe (Map2D Char)
moveH p d h = let p0 = pMove p d in
  case M.lookup p0 h of
    Nothing -> Just $ mMove p p0 h
    Just '#' -> Nothing
    Just 'O' -> case moveH p0 d h of
                  Nothing -> Nothing
                  Just h0 -> Just $ mMove p p0 h0
    _ -> error ("unexpected character at "++show p0)

moveR :: Point -> Direction -> Map2D Char -> (Point,Map2D Char)
moveR p d h = case moveH p d h of
  Nothing -> (p,h)
  Just h0 -> (pMove p d,h0)

stepMoves :: Point -> String -> Map2D Char -> IO ()
stepMoves p [] h = putStrLn (showMap id h)
stepMoves p (m:ms) h  = do
    putStrLn (showMap id h)
    putStrLn (show p)
    let d = mDir m
        (p0,h0) = moveR p d h
    stepMoves p0 ms h0

movesR :: Point -> String -> Map2D Char -> Map2D Char
movesR p [] h = h
movesR p (m:ms) h = let d = mDir m
                        (p0,h0) = moveR p d h
                    in movesR p0 ms h0

gps :: Point -> Int
gps (x,y) = 100*y + x

part1 :: Map2D Char -> String -> Int
part1 h ms = sum $ map gps $ mFind 'O' (movesR (robot h) ms h)

-- Part 2

part2 :: Map2D Char -> String -> Int
part2 h ms = 2

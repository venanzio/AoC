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
  putStrLn (showMap id h)
  putStrLn (show $ robot h)
  putStrLn ms
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

move :: Point -> Direction -> Map2D Char -> Map2D Char
move p d h = let p0 = pMove p d in
  case M.lookup p0 h of
    Nothing -> undefined

part1 :: Map2D Char -> String -> Int
part1 h ms = 1

-- Part 2

part2 :: Map2D Char -> String -> Int
part2 h ms = 2

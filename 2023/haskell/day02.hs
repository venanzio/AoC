-- Advent of Code 2023, day 2
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

data Colours = Colours {
  { cRed :: Int
  , cGreen :: Int
  , cBlue :: Int
  }

noColours = Colour {cRed = 0, cGreen = 0, cBlue = 0}

setColour :: Colours -> String -> Int -> Colours
setColour cols c x = case c of
      "red" -> cols {cRed = x}
      "green" -> cols {cGreen = x}
      "blue" -> cols {cBlue = x}

extraction :: Parser [(String,Int)]
extaction = someSep (do col <- word
                        n <- natural
                        return (col,n))
                    (symbol ",")
  
pColours :: Parser Colours
pColours = do cs <- extraction
                return setColoursAux noColours cs
  where setColoursAux cols [] = cols
        setColoursAux cols (c,x):cs = setColoursAux (setColour cols c x) cs
  
pCube :: String -> Parser Int
pCube color = undefined

pCubes :: Parser (Int,Int,Int)
pCubes = undefined

pGame :: Parser [(Int,Int,Int)]
pGame = undefined

pData :: Parser ()
pData = return ()

pInput :: Parser [()]
pInput = pLines pData

-- Part 1

part1 :: [()] -> Int
part1 _ = 1

-- Part 2

part2 :: [()] -> Int
part2 _ = 2

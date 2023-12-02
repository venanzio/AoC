-- Advent of Code 2023, day 2
--  Venanzio Capretta

module Main where

import System.Environment

import Control.Applicative
import FunParser

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn (show (xs!!0))
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

data Colours = Colours
  { cRed :: Int
  , cGreen :: Int
  , cBlue :: Int
  }
  deriving Show

noColours = Colours {cRed = 0, cGreen = 0, cBlue = 0}

setColour :: Colours -> String -> Int -> Colours
setColour cols c x = case c of
      "red" -> cols {cRed = x}
      "green" -> cols {cGreen = x}
      "blue" -> cols {cBlue = x}

pExtraction :: Parser [(String,Int)]
pExtraction = someSep (do n <- natural
                          col <- word
                          return (col,n))
                      (symbol ",")
  
pColours :: Parser Colours
pColours = do cs <- pExtraction
              return (setColoursAux noColours cs)
  where setColoursAux cols [] = cols
        setColoursAux cols ((c,x):cs) = setColoursAux (setColour cols c x) cs
  
pGame :: Parser [Colours]
pGame = someSep pColours (symbol ";")

pInput :: Parser [[Colours]]
pInput= many (do symbol "Game"
                 natural
                 symbol ":"
                 pGame)

-- Part 1

maxRed = 12
maxGreen = 13
maxBlue = 14

possibleCols :: Colours -> Bool
possibleCols c = (cRed c <= maxRed) && (cGreen c <= maxGreen) && (cBlue c <= maxBlue)

possibleGame :: [Colours] -> Bool
possibleGame = all possibleCols

possible :: [[Colours]] -> Int
possible = possibleAux 1 0 where
  possibleAux n s [] = s
  possibleAux n s (g:gs) =
    if (possibleGame g)
    then possibleAux (n+1) (s+n) gs
    else possibleAux (n+1) s gs

possibleSum :: Int -> Int -> [Colours] -> Int
possibleSum s n [] = s
possibleSum s n (g:gs) =
  if possibleCols g then possibleSum (s+n) (n+1) gs
                    else possibleSum s (n+1) gs

part1 :: [[Colours]] -> Int
part1 = possibleSum 0 1 . map minColours 

-- Part 2

minColours :: [Colours] -> Colours
minColours [] = noColours
minColours (c:cs) = let c0 = minColours cs in
  Colours { cRed = max (cRed c) (cRed c0)
          , cGreen = max (cGreen c) (cGreen c0)
          , cBlue = max (cBlue c) (cBlue c0)
          }

power :: Colours -> Int
power c = (cRed c)*(cGreen c)*(cBlue c)

part2 :: [[Colours]] -> Int
part2 = sum . map (power . minColours)

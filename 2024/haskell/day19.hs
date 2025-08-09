-- Advent of Code 2024, day 19
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
-- import Data.Char
import Control.Applicative
import qualified Data.Map as M

import FunParser
-- import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let (patterns',designs) = parseAll pInput input
      patterns = reverse $ sortOn length patterns'
  putStrLn ("Part 1: " ++ show (part1 patterns designs))
  putStrLn ("Part 2: " ++ show (part2 patterns designs))

-- Parsing the input

pPatterns :: Parser [String]
pPatterns = someSepStr word ","

pDesigns :: Parser [String]
pDesigns = some word

pInput :: Parser ([String],[String])
pInput = do patterns <- pPatterns
            designs <- pDesigns
            return (patterns,designs)
            
-- Part 1

sublists = concat. map inits1 . tails
  where inits1 [] = [[]]
        inits1 (x:xs) = map (x:) (inits xs)

correct :: [String] -> String -> Bool
correct pats des = correctTable M.! des
  where correctTable = M.fromList [(sub,subOK sub) | sub <- nub (sublists des)]
        subOK [] = True
        subOK design = any (correctTable M.!)
          [pTail | Just pTail <- map (\p -> stripPrefix p design) pats]
        

        
part1 :: [String] -> [String] -> Int
part1 patterns designs = length (filter (correct patterns) designs)

-- Part 2

part2 ::  [String] -> [String] -> Int
part2 _ _ = 2

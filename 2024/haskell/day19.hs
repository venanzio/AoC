-- Advent of Code 2024, day 19
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
-- import Data.Char
import Control.Applicative
-- import qualified Data.Map as M

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
  putStrLn (show patterns)
  putStrLn (show designs)
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

mkDes :: [String] -> String -> Bool
mkDes patterns [] = True
mkDes patterns design = any (mkDes patterns)
  [pTail | Just pTail <- map (\p -> stripPrefix p design) patterns]


correctDes :: [String] -> String -> Bool
correctDes pats [] = True
correctDes []   des = False
correctDes (pat:pats) des = any splitCorrect (cutInfix pat des) ||
                            correctDes pats des
    where splitCorrect (pre,post) = correctDes pats pre &&
                                    correctDes (pat:pats) post
  
cutInfix :: String -> String -> [(String,String)]
cutInfix pat des =
  [prepost | Just prepost <- map (\i -> cutInfixAt i pat des) [0..length des]]

cutInfixAt :: Int -> String -> String -> Maybe (String,String)
cutInfixAt i pat des = let (pre,rest) = splitAt i des
                       in stripPrefix pat rest >>= \post -> return (pre,post)

part1 :: [String] -> [String] -> Int
part1 patterns designs = length (filter (correctDes patterns) designs)

-- Part 2

part2 ::  [String] -> [String] -> Int
part2 _ _ = 2

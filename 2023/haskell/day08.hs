-- Advent of Code 2023, day 8
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
      (inst,net) = xs
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn (show (take 10 (allPaths net (ouroboros inst) 0 "AAA" endNode)))
  -- putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type Instructions = String
type Network = M.Map String (String,String)

pInstructions :: Parser Instructions
pInstructions = many (char 'L' <|> char 'R')

pNode :: Parser (String,(String,String))
pNode = do s <- word
           symbol "= ("
           tL <- word
           symbol ","
           tR <- word
           symbol ")"
           return (s,(tL,tR))

pNetwork :: Parser Network
pNetwork = many pNode >>= return . M.fromList

pInput :: Parser (Instructions,Network)
pInput = pPair pInstructions pNetwork

-- Part 1

step :: Network -> Char -> String -> String
step net 'L' s = fst $ unJust $ M.lookup s net
step net 'R' s = snd $ unJust $ M.lookup s net


navigate :: Network -> Instructions ->
            Int -> String -> (String -> Bool) -> Int
navigate net inst steps s end =
  if end s
  then steps
  else navigate net (tail inst) (steps+1) (step net (head inst) s) end

ouroboros :: [a] -> [a]
ouroboros xs = xs ++ ouroboros xs

part1 :: (Instructions,Network) -> Int
part1 (inst,net) = navigate  net (ouroboros inst) 0 "AAA" (=="ZZZ")

-- Part 2

startNodes :: Network -> [String]
startNodes = filter (\s -> s!!2 == 'A') . M.keys

allZ :: [String] -> Bool
allZ = all (\s -> s!!2 == 'Z')

endNode :: String -> Bool
endNode s = s!!2 == 'Z'

navigate2 :: Network -> Int -> [String] -> Instructions -> Int
navigate2 net steps ss inst =
  if allZ ss
  then steps
  else navigate2 net (steps+1) (map (step net (head inst)) ss) (tail inst)

lcmL :: (Integral a) => [a] -> a
lcmL = foldr lcm 1

allPaths :: Network -> Instructions ->
            Int -> String -> (String -> Bool) -> [Int]
allPaths net inst steps s end =
  let paths = allPaths net (tail inst) (steps+1) (step net (head inst) s) end
  in if end s then steps : paths else paths
  

part2 :: (Instructions,Network) -> Int
part2 (inst,net) =
  lcmL (map (\s -> navigate net (ouroboros inst) 0 s endNode)
            (startNodes net))

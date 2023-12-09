-- Advent of Code 2023, day 8
--  Venanzio Capretta

module Main where

import System.Environment
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
  let (inst,net) = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 net inst))
  putStrLn ("Part 2: " ++ show (part2 net inst))
  putStrLn ("\nSteps and Z nodes reached from any A node:")
  putStrLn (concat $
            map (\s -> "Start at " ++ s ++ " : " ++
                       show (take 5 (stepsZ net (ouroboros inst) 0 s endNode)) ++ "\n")
                (startNodes net))

-- Parsing the input

type Instructions = String
type Node = String
type Network = M.Map Node (Node,Node)

pInstructions :: Parser Instructions
pInstructions = many (char 'L' <|> char 'R')

pNode :: Parser (Node,(Node,Node))
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

step :: Network -> Char -> Node -> Node
step net 'L' s = fst $ unJust $ M.lookup s net
step net 'R' s = snd $ unJust $ M.lookup s net

navigate :: Network -> Instructions ->
            Int -> Node -> (Node -> Bool) -> Int
navigate net inst steps s end =
  if end s
  then steps
  else navigate net (tail inst) (steps+1) (step net (head inst) s) end

part1 :: Network -> Instructions -> Int
part1 net inst = navigate  net (ouroboros inst) 0 "AAA" (=="ZZZ")

-- Part 2

startNodes :: Network -> [Node]
startNodes = filter (\s -> s!!2 == 'A') . M.keys

endNodes  :: Network -> [Node]
endNodes = filter (\s -> s!!2 == 'Z') . M.keys

allZ :: [Node] -> Bool
allZ = all (\s -> s!!2 == 'Z')

endNode :: Node -> Bool
endNode s = s!!2 == 'Z'

-- This is the brute force solution for Part 2
navigate2 :: Network -> Int -> [Node] -> Instructions -> Int
navigate2 net steps ss inst =
  if allZ ss
  then steps
  else navigate2 net (steps+1) (map (step net (head inst)) ss) (tail inst)

-- This solution only works because of the input has a simple structure
part2 :: Network -> Instructions -> Int
part2 net inst =
  lcmL (map (\s -> navigate net (ouroboros inst) 0 s endNode)
            (startNodes net))


stepsZ :: Network -> Instructions ->
          Int -> Node -> (Node -> Bool) -> [(Int,Node)]
stepsZ net inst steps s end =
  if end s
  then (steps,s) : stepsZ net (tail inst) 1 (step net (head inst) s) end
  else stepsZ net (tail inst) (steps+1) (step net (head inst) s) end
  

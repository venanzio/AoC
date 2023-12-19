-- Advent of Code 2023, day 19
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
  let (ws,ps) = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 ws ps))
  putStrLn ("Part 2: " ++ show (part2 ws ps))

-- Parsing the input

data Part = Part {xP::Int,mP::Int,aP::Int,sP::Int}
  deriving Show

data Destination = Send String | Accept | Reject
  deriving Show

data Rule = Rule {
  categoryR :: Part -> Int,
  intervalR : Interval
--  relationR :: Int -> Int -> Bool,
--  valueR :: Int,
  destinationR :: Destination}

type Workflow = ([Rule],Destination)

pDestination :: Parser Destination
pDestination = (symbol "A" >> return Accept) <|> (symbol "R" >> return Reject)
                  <|> (word >>= return.Send)

pRule :: Parser Rule
pRule = do
  category <- ((symbol "x" >> return xP) <|> (symbol "m" >> return mP)
               <|> (symbol "a" >> return aP) <|> (symbol "s" >> return sP))
  interval <- (char '<' >> integer >>= \v (0,v-1))
              <|> (char '>' >> integer >>= \v (v+1,4000))
  symbol ":"
  destination <- pDestination
  return (Rule category interval destination)

pWorkflow :: Parser (String,Workflow)
pWorkflow = do name <- word
               symbol "{"
               rs <- many (pRule >>= \r -> symbol "," >> return r) 
               endRule <- pDestination
               symbol "}"
               return (name,(rs,endRule))

pPart :: Parser Part
pPart = do symbol "{x="
           x <- integer
           symbol ",m="
           m <- integer
           symbol ",a="
           a <- integer
           symbol ",s="
           s <- integer
           symbol "}"
           return (Part x m a s)

pInput :: Parser ([(String,Workflow)],[Part])
pInput = do ws <- some pWorkflow
            ps <- some pPart
            return (ws,ps)

-- Part 1

workflow :: Workflow -> Part -> Destination
workflow ([],end) p = end
workflow (r:rs,end) p =
  if (categoryR r p) inInterval (intervalR r)
  then destinationR r
  else workflow (rs,end) p

exec :: [(String,Workflow)] -> Part -> Bool
exec ws p = execFrom "in" where
  execFrom wName = let (Just w) = lookup wName ws in
    case workflow w p of
      Send n -> execFrom n
      Accept -> True
      Reject -> False

partRating :: Part -> Int
partRating p = xP p + mP p + aP p + sP p

part1 :: [(String,Workflow)] -> [Part] -> Int
part1 ws ps = sum $ map partRating $ filter (exec ws) ps

-- Part 2

data PRange = Part {xP:Range,mP::Range,aP::Range,sP::Range}
  deriving Show

initRange :: PRange
initRange = PRange [(1,4000)] [(1,4000)] [(1,4000)] [(1,4000)]

ruleRange :: Rule -> PRange -> PRange
ruleRange = undefined

part2 :: [(String,Workflow)] -> [Part]  -> Int
part2 ws ps = 2

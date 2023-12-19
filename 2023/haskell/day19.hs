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
  putStrLn ("Part 2: " ++ show (part2 ws))

-- Parsing the input

data Category = X | M | A | S
  deriving Show

type Part = (Int,Int,Int,Int)

catVal :: Category -> Part -> Int
catVal X (x,_,_,_) = x
catVal M (_,m,_,_) = m
catVal A (_,_,a,_) = a
catVal S (_,_,_,s) = s

data Destination = Send String | Accept | Reject
  deriving Show

data Rule = Rule {
  categoryR :: Category,
  intervalR :: Interval,
--  relationR :: Int -> Int -> Bool,
--  valueR :: Int,
  destinationR :: Destination}
  deriving Show

type Workflow = ([Rule],Destination)

pDestination :: Parser Destination
pDestination = (symbol "A" >> return Accept) <|> (symbol "R" >> return Reject)
                  <|> (word >>= return.Send)

pRule :: Parser Rule
pRule = do
  category <- ((symbol "x" >> return X) <|> (symbol "m" >> return M)
               <|> (symbol "a" >> return A) <|> (symbol "s" >> return S))
  interval <- (char '<' >> integer >>= \v -> return (0,v-1))
              <|> (char '>' >> integer >>= \v -> return (v+1,4000))
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
           return (x,m,a,s)

pInput :: Parser ([(String,Workflow)],[Part])
pInput = do ws <- some pWorkflow
            ps <- some pPart
            return (ws,ps)

-- Part 1

workflow :: Workflow -> Part -> Destination
workflow ([],end) p = end
workflow (r:rs,end) p =
  if catVal (categoryR r) p `inInterval` (intervalR r)
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
partRating (x,m,a,s) = x+m+a+s

part1 :: [(String,Workflow)] -> [Part] -> Int
part1 ws ps = sum $ map partRating $ filter (exec ws) ps

-- Part 2

type PRange = (Range,Range,Range,Range)

catRange :: Category -> PRange -> Range
catRange X (x,_,_,_) = x
catRange M (_,m,_,_) = m
catRange A (_,_,a,_) = a
catRange S (_,_,_,s) = s

emptyPR :: PRange -> Bool
emptyPR (x,m,a,s) = x==[] || m==[] || a==[] || s == []

changeCat :: Category -> Range -> PRange -> PRange
changeCat _ [] _ = ([],[],[],[])
changeCat X range (mX,mR,aR,sR) = (range,mR,aR,sR)
changeCat M range (mX,mR,aR,sR) = (mX,range,aR,sR)
changeCat A range (mX,mR,aR,sR) = (mX,mR,range,sR)
changeCat S range (mX,mR,aR,sR) = (mX,mR,aR,range)


initRange :: PRange
initRange = ([(1,4000)],[(1,4000)],[(1,4000)],[(1,4000)])

ruleRange :: Rule -> PRange -> (PRange,PRange)
ruleRange rule pRange =
  let cat = categoryR rule
      range = catRange cat pRange
      rrange = rIntersection range [intervalR rule]
  in (changeCat cat rrange pRange, changeCat cat (rDiff range rrange) pRange)
      
workflowR :: Workflow -> PRange -> [(Destination,PRange)]
workflowR _ pRange | emptyPR pRange = []
workflowR ([],end) pRange = [(end,pRange)]
workflowR (r:rs,end) pRange =
  let (rRange,cRange) = ruleRange r pRange in
     (destinationR r,rRange) : workflowR (rs,end) cRange

sizePRange :: PRange -> Int
sizePRange (x,m,a,s) = sizeRange x * sizeRange m * sizeRange a * sizeRange s

workflowA ::  [(String,Workflow)] -> Destination -> PRange -> Int
workflowA ws Accept pRange = sizePRange pRange
workflowA ws Reject pRange = 0
workflowA ws (Send name) pRange =
  let Just w = lookup name ws
  in sum [workflowA ws dest dRange | (dest,dRange) <- workflowR w pRange]

part2 :: [(String,Workflow)] -> Int
part2 ws = workflowA ws (Send "in") initRange

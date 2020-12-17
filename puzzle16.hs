module PuzzleInput where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M


puzzle :: String -> IO Int
puzzle fileName = do
  input <- readFile fileName
  let [(rules,input1)] = parse (some pRule) input
      [(yourT,input2)] = parse pYTicket input1
      [(tickets,_)] = parse pNBTickets input2
      ts = filter (\t -> violations rules t == []) tickets
      rs = rulesIdx rules ts
      drs = depRules rs
  -- return (sum $ concat $ map (violations rules) tickets)
  return (product $ depFields drs yourT)

type Range = (Int,Int)
type TicketRule = (String,Range,Range)
type Ticket = [Int]


pRule :: Parser TicketRule
pRule = do field <- some (sat (/=':'))
           symbol ":"
           start1 <- natural
           symbol "-"
           end1 <- natural
           symbol "or"
           start2 <- natural
           symbol "-"
           end2 <- natural
           return (field,(start1,end1),(start2,end2))
  
pTicket :: Parser Ticket
pTicket = seqSep1 natural ","

pYTicket :: Parser Ticket
pYTicket = symbol "your ticket:" >> pTicket

pNBTickets :: Parser [Ticket]
pNBTickets = symbol "nearby tickets:" >> some pTicket

                 
-- Part 1

ruleCheck :: TicketRule -> Int -> Bool
ruleCheck (_,(s1,e1),(s2,e2)) x = (s1 <= x && x <= e1) || (s2 <= x && x <= e2)

checkSome :: [TicketRule] -> Int -> Bool
checkSome rs x = or $ map (\r -> ruleCheck r x) rs

violations :: [TicketRule] -> Ticket -> [Int]
violations rs = filter (not . checkSome rs)

-- Part 2

rVerify :: TicketRule -> [Int] -> Bool
rVerify r xs = all (ruleCheck r) xs

rIndexs :: TicketRule -> [Ticket] -> [Int]
rIndexs r ts = [i | i <- [0..length (head ts) - 1],
                     rVerify r (map (!!i) ts)]

riPairs :: [TicketRule] -> [Ticket] -> [(Int,Int)]
riPairs rs ts = [(j,i) | j <- [0..length rs -1], i <- rIndexs (rs!!j) ts]

delPair :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
delPair (i,j) = filter (\(i',j') -> i'/=i && j'/=j)

pairing :: [(Int,Int)] -> Int -> Maybe [(Int,Int)]
pairing _ 0 = Just []
pairing [] _ = Nothing
pairing ps l = let (p,ps') = extract ps in
  case pairing ps' (l-1) of
  Just ps'' -> Just (p:ps'')
  Nothing -> pairing (delete p ps) l

rulesIdx ::  [TicketRule] -> [Ticket] -> [(TicketRule,Int)]
rulesIdx rs ts = let Just ps = pairing (riPairs rs ts) (length rs)
                 in [(rs!!j,i) | (j,i) <- ps]

-- Extract the element that causes the least simplification (leaves more pairs)
extract :: [(Int,Int)] -> ((Int,Int),[(Int,Int)])
extract ps = maximumBy (\p1 p2 -> compare (length (snd p1)) (length (snd p2)))
                       [(p,delPair p ps) | p <- ps]

depRules :: [(TicketRule,Int)] -> [(TicketRule,Int)]
depRules = filter checkDepR
  where checkDepR ((field,_,_),_) = take 9 field == "departure"

dRules :: [TicketRule] -> [TicketRule]
dRules = filter checkDR
  where checkDR (field,_,_) = take 9 field == "departure"

depFields :: [(TicketRule,Int)] -> Ticket -> [Int]
depFields rs t = [t!!i | (_,i) <- rs]


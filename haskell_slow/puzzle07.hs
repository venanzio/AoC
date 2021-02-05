-- Advent of Code 2020, day 7

module Main where

import System.Environment
import Data.List
import Data.Char


import Control.Applicative

import qualified Data.Map as M
import qualified Data.Set as S


import FunParser

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let rules = parseAll (some pBRule) input -- parseAll pBRules input
      -- bc = bagContain rules
      mybag = Bag "shiny" "gold"
      bs = containers rules mybag --bagSupRec bc mybag
  putStrLn ("Part 1: " ++ show (S.size bs))
  putStrLn ("Part 2: ") -- ++ show (numBags rules mybag))


-- Data Structures for Bags and Bag Rules

data Bag = Bag String String   -- Adjective and color
  deriving (Show,Eq,Ord)

data BagR = BagR Bag [(Int,Bag)]  -- Bag rule: container and contained
  deriving (Show,Eq)

type BagRules = M.Map Bag [(Int,Bag)]

data BagT = BagT Bag [BagT]  -- Bag Membership Tree (node contained in children)
  deriving (Show,Eq)

-- Parse a bag name (adjective and color)
pBag :: Parser Bag
pBag = do adj <- word
          col <- word
          (symbol "bags" <|> symbol "bag")
          return (Bag adj col)

-- Parse number and bag name
pNBag :: Parser (Int,Bag)
pNBag = do n <- natural
           bag <-pBag
           return (n,bag)

-- Parse a bag rule
pBRule :: Parser BagR
pBRule = do cont <- pBag
            symbol "contain"
            bags <- (symbol "no" >> symbol "other" >> symbol "bags"
                     >> return [])
                    <|> (seqSep pNBag ",")
            symbol "."
            return (BagR cont bags)
            
pBRules :: Parser BagRules
pBRules = (do
  BagR cont bags <- pBRule
  rs <- pBRules
  return (M.insert cont bags rs)
  ) <|> return M.empty

-- Part 1

-- all the bags recursively contained in one bag
contents :: BagRules -> M.Map Bag (S.Set Bag)
contents rs = conts where
  conts = M.map hconts rs
  hconts :: [(Int,Bag)] -> S.Set Bag
  hconts ibs = foldl (\s (i,b) -> S.union s (conts M.! b))
                     (S.fromList $ map snd ibs) ibs

-- whether a bag is contained recursively
contains :: BagRules -> Bag -> M.Map Bag Bool
contains rs b = cont where
  cont = M.map hcont rs
  hcont :: [(Int,Bag)] -> Bool
  hcont ibs = b `elem` (map snd ibs) || any (\(i,b') -> cont M.! b') ibs


{-


-- Test if a bag contains any of a set of bags
contains :: BagR -> S.Set Bag -> Bool
contains (BagR bag conts) bs =
  any (\(_,b) -> S.member b bs) conts

-- Add to a set the bags that contain some of those in the set
conts :: S.Set Bag -> [BagR] -> S.Set Bag
conts = foldr (\r@(BagR b _) bs -> if contains r bs then S.insert b bs else bs)
                   

-- set of bags that recursively contain a given bag
containers :: [BagR] -> Bag -> S.Set Bag
containers rs b = conts (S.insert b (containers rs b)) rs
-}


{-

-- list of pairs: first bag contains the second
type BCont = [(Bag,Bag)]

bcUpdate :: Bag -> Bag -> BCont -> BCont
bcUpdate b1 b2 bc = (b1,b2):bc

bcBagR :: BagR -> BCont-> BCont
bcBagR (BagR b nbs) bc = foldr (\b' bc' -> bcUpdate b b' bc') bc (map snd nbs)
  
bagContain :: [BagR] -> BCont
bagContain = nub . foldr bcBagR []


bagSup :: BCont -> Bag -> [Bag]
bagSup bc b = map fst $ filter (\(b1,b2) -> b2==b) bc

eqBags :: [Bag] -> [Bag] -> Bool
eqBags b1 b2 = sort (nub b1) == sort (nub b2)


bagSupRec :: BCont -> Bag -> [Bag]
bagSupRec bc b = bagSR (bagSup bc b) where
  bagSR bs = let bs' = nub $ bs ++ (concat $ map (bagSup bc) bs)
             in if (bs' `eqBags` bs)
                then bs
                else bagSR bs'                   

-- Part 2

contents :: [BagR] -> Bag -> [(Int,Bag)]
contents [] _ = []
contents (BagR b nbs : brs) b' = if b==b' then  nbs else contents brs b'

numBags :: [BagR] -> Bag -> Int
numBags br b =
  let nbs = contents br b
      n = sum (map fst nbs)
      m = sum (map (\(i,b') -> i*(numBags br b')) nbs)
  in (n+m)
  
-}

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
  let rules = parseAll pBRules input
      bc = bagContain rules
      mybag = Bag "shiny" "gold"
      bs = bagSupRec bc mybag
  putStrLn ("Part 1: " ++ show (length bs))
  putStrLn ("Part 2: ") -- ++ show (numBags rules mybag))


-- Data Structures for Bags and Bag Rules

data Bag = Bag String String   -- Adjective and color
  deriving (Show,Eq,Ord)

data BagR = BagR Bag [(Int,Bag)]  -- Bag rule: container and contained
  deriving (Show,Eq)

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

pBRules :: Parser [BagR]
pBRules = some pBRule

-- Part 1

-- Represent a relation on type a as a map from a to subsets of a
--  (May want to check Data.Relation)
type Relation a = M.Map a (S.Set a)

-- Relation from a list of pairs
listR :: Ord a => [(a,a)] -> Relation a
listR = foldr (\(x,y) -> M.insertWith S.union x (S.singleton y)) M.empty

-- Image of an element
image :: Ord a => Relation a -> a -> S.Set a
image r a = case M.lookup a r of
  Nothing -> S.empty
  Just s  -> s

-- transitive closure
trans :: Ord a => Relation a -> Relation a
trans r = tr where
  tr = M.map images r
  images as = S.union as (S.unions (S.map (image tr) as))








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

{-

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

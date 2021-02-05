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
      cr = contains rules  -- contain relation
      rc = inverse cr      -- "is contained" relation
      mybag = Bag "shiny" "gold"
      bs = image (trans rc) mybag  -- bags that may contain mybag
  putStrLn ("Part 1: " ++ show (length bs))
  putStrLn ("Part 2: " ++ show (numBags rules M.! mybag))


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

-- inverse relation
inverse :: Ord a => Relation a -> Relation a
inverse = M.foldrWithKey
            (\b s r ->
               S.foldl (\r' b' -> M.insertWith S.union b' (S.singleton b) r')
                       r s)
            M.empty

-- transitive closure
trans :: Ord a => Relation a -> Relation a
trans r = tr where
  tr = M.map images r
  images as = S.union as (S.unions (S.map (image tr) as))

-- Relation container-content for bags
contains :: [BagR] -> Relation Bag
contains = foldr (\(BagR b ibs) -> M.insert b (S.fromList (map snd ibs))) M.empty


-- Part 2

-- Finite map from bags to the number of recursive bags they contained
-- Solved using dynamic programming and lazy evaluation

numBags :: [BagR] -> M.Map Bag Int
numBags rs = nb where
  nb = M.fromList [(b, sum [i*(1 + nb M.! b') | (i,b')<-ibs])
                  | (BagR b ibs) <- rs]

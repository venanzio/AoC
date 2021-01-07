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
  let rules = parseAll pBRules input
      bc = bagContain rules
      mybag = Bag "shiny" "gold"
      bs = bagSupRec bc mybag
  -- return (length bs)
  return (numBags rules mybag)

data Bag = Bag String String   -- Adjective and color
  deriving (Show,Eq,Ord)

data BagR = BagR Bag [(Int,Bag)]  -- Bag rule: container and contained
  deriving (Show,Eq)

data BagT = BagT Bag [BagT]  -- Bag Membership Tree (node contained in children)
  deriving (Show,Eq)

pBag :: Parser Bag
pBag = do adj <- word
          col <- word
          return (Bag adj col)

pNBag :: Parser (Int,Bag)
pNBag = do n <- natural
           bag <-pBag
           (symbol "bags" <|> symbol "bag")
           return (n,bag)

pBRule :: Parser BagR
pBRule = do cont <- pBag
            symbol "bags"
            symbol "contain"
            bags <- (symbol "no" >> symbol "other" >> symbol "bags"
                     >> return [])
                    <|> (seqSep pNBag ",")
            symbol "."
            return (BagR cont bags)
            
pBRules :: Parser [BagR]
pBRules = some pBRule

-- Part 1

noDups :: Eq a => [a] -> [a]
noDups = foldr (\x ys -> if x `elem` ys then ys else x:ys) []


-- list of pairs: first bag contains the second
type BCont = [(Bag,Bag)]

bcUpdate :: Bag -> Bag -> BCont -> BCont
bcUpdate b1 b2 bc = (b1,b2):bc

bcBagR :: BagR -> BCont-> BCont
bcBagR (BagR b nbs) bc = foldr (\b' bc' -> bcUpdate b b' bc') bc (map snd nbs)
  
bagContain :: [BagR] -> BCont
bagContain = noDups . foldr bcBagR []


bagSup :: BCont -> Bag -> [Bag]
bagSup bc b = map fst $ filter (\(b1,b2) -> b2==b) bc

eqBags :: [Bag] -> [Bag] -> Bool
eqBags b1 b2 = sort (noDups b1) == sort (noDups b2)


bagSupRec :: BCont -> Bag -> [Bag]
bagSupRec bc b = bagSR (bagSup bc b) where
  bagSR bs = let bs' = noDups $ bs ++ (concat $ map (bagSup bc) bs)
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
  

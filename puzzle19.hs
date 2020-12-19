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
  let [(rs,src)] = parse rules input
      msgs = parseAll (some word) src
      p = rParse rs 0
  return (countValid p msgs)

data Rule = Term Char | NTerm [[Int]]
  deriving (Eq,Show)
type IR = M.Map Int Rule
type Code = M.Map Int (Parser String)


rules :: Parser IR
rules = blocks rule >>= return . M.fromList

rule :: Parser (Int,Rule)
rule = do
  n <- natural
  symbol ":"
  r <- (termRule <|> nTermRule)
  return (n,r)

termRule :: Parser Rule
termRule = do
  symbol "\""
  c <- item
  symbol "\""
  return (Term c)

nTermRule :: Parser Rule
nTermRule = seqSep1 (some natural) "|" >>= return . NTerm


rParse :: IR -> Int -> Parser String
-- added for part 2
rParse ir 8 = do ss <- someP (rParse ir 42)
                 return (concat ss)
rParse ir 11 = do ss1 <- some (rParse ir 42)
                  ss2 <- repN (length ss1) (rParse ir 31)
                  return (concat $ ss1++ss2)
-- part 1
rParse ir n = case M.lookup n ir of
  Nothing -> empty
  Just (Term c) -> string [c]
  Just (NTerm rss) -> altR ir rss

altR :: IR -> [[Int]] -> Parser String
altR ir = parallel . map (seqR ir)

seqR :: IR -> [Int] -> Parser String
seqR ir = seqStr . map (rParse ir)

seqStr :: [Parser String] -> Parser String
seqStr = fmap concat . sequence


checkP :: Parser a -> String -> Bool
checkP p s =
  any (\(_,s) -> s=="") (parse p s)

countValid :: Parser a -> [String] -> Int
countValid p = length . valid p

valid :: Parser a -> [String] -> [String]
valid p = filter (checkP p)



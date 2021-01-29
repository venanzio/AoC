-- Advent of Code 2020, day 4

module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let ps = parseAll passports input
  putStrLn ("Part 1: " ++ show (countValidPass ps))
  putStrLn ("Part 2: " ++ show (countValid ps))

data Passport = Pass 
  { byrP :: Maybe String,
    iyrP :: Maybe String,
    eyrP :: Maybe String,
    hgtP :: Maybe String,
    hclP :: Maybe String,
    eclP :: Maybe String,
    pidP :: Maybe String,
    cidP :: Maybe String
  } deriving (Show,Eq)

showPassports :: [Passport] -> String
showPassports ps = concat $
  map (\(i,p) -> "Passport "++(show i)++"\n"++(show p)++"\n\n")
  (zip [0..] ps)


emptyPass :: Passport
emptyPass = Pass {
  byrP = Nothing, 
  iyrP = Nothing,
  eyrP = Nothing,
  hgtP = Nothing,
  hclP = Nothing,
  eclP = Nothing,
  pidP = Nothing,
  cidP = Nothing
  }


-- part 1

validPass :: Passport -> Bool
validPass p = byrP p /= Nothing &&
              iyrP p /= Nothing &&
              eyrP p /= Nothing &&
              hgtP p /= Nothing &&
              hclP p /= Nothing &&
              eclP p /= Nothing &&
              pidP p /= Nothing 
  
countValidPass :: [Passport] -> Int
countValidPass = length . (filter validPass)



-- part 2

maybeParse :: Parser a -> String -> Maybe a
maybeParse pa s = case parse pa s of
  [(a,"")] -> Just a
  _        -> Nothing

valid :: Passport -> Bool
valid p = validByr p && validIyr p && validEyr p && validHgt p &&
          validHcl p && validEcl p && validPid p

countValid :: [Passport] -> Int
countValid = length . (filter valid)

validYear :: Int -> Int -> String -> Bool
validYear low high s = length s == 4 && checkYear (maybeParse natural s)
  where checkYear Nothing = False
        checkYear (Just n) = low <= n && n <= high

isNum :: String -> Maybe Int
isNum s = case (parse natural s) of
            [(n,"")] -> Just n
            _ -> Nothing

validByr :: Passport -> Bool
validByr p = case (byrP p) of
  Nothing -> False
  Just s -> validYear 1920 2002 s

validIyr :: Passport -> Bool
validIyr p = case (iyrP p) of
  Nothing -> False
  Just s -> validYear 2010 2020 s

validEyr :: Passport -> Bool
validEyr p = case (eyrP p) of
  Nothing -> False
  Just s -> validYear 2020 2030 s

validHgt :: Passport -> Bool
validHgt p = case (hgtP p >>= maybeParse parseHgt) of
  Just (n,"cm") -> 150 <= n && n <= 293
  Just (n,"in") -> 59 <= n && n <= 76
  _ -> False
  
parseHgt :: Parser (Int,String)
parseHgt = do
  n <- natural
  unit <- symbol "cm" <|> symbol "in"
  return (n,unit)  


validHcl :: Passport -> Bool
validHcl p = case (hclP p >>= maybeParse parseHcl) of
  Just s -> length s == 6
  Nothing -> False

parseHcl :: Parser String
parseHcl = do
  char '#'
  many $ sat (`elem` "0123456789abcdef")

validEcl :: Passport -> Bool
validEcl p = (eclP p) `elem` (map Just ["amb","blu","brn","gry","grn","hzl","oth"])

validPid :: Passport -> Bool
validPid p = case pidP p of
  Nothing -> False
  Just s -> length s == 9 && (all isDigit s)



-- Parsing the input

passports :: Parser [Passport]
passports = blocks pass

pass :: Parser Passport
pass = (byr >>= addByr)
       <|> (iyr >>= addIyr)
       <|> (eyr >>= addEyr)
       <|> (hgt >>= addHgt)
       <|> (hcl >>= addHcl)
       <|> (ecl >>= addEcl)
       <|> (pid >>= addPid)
       <|> (cid >>= addCid)
       <|> (space >> return emptyPass)
  
byr :: Parser String
byr = do
  symbol "byr:"
  label

addByr :: String -> Parser Passport
addByr b = pass >>= \p -> return $ p {byrP = Just b}

iyr :: Parser String
iyr = do
  symbol "iyr:"
  label

addIyr :: String -> Parser Passport
addIyr b = pass >>= \p -> return $ p {iyrP = Just b}

eyr :: Parser String
eyr = do
  symbol "eyr:"
  label

addEyr :: String -> Parser Passport
addEyr b = pass >>= \p -> return $ p {eyrP = Just b}

hgt :: Parser String
hgt = do
  symbol "hgt:"
  label

addHgt :: String -> Parser Passport
addHgt b = pass >>= \p -> return $ p {hgtP = Just b}

hcl :: Parser String
hcl = do
  symbol "hcl:"
  label

addHcl :: String -> Parser Passport
addHcl b = pass >>= \p -> return $ p {hclP = Just b}

ecl :: Parser String
ecl = do
  symbol "ecl:"
  label

addEcl :: String -> Parser Passport
addEcl b = pass >>= \p -> return $ p {eclP = Just b}

pid :: Parser String
pid = do
  symbol "pid:"
  label

addPid :: String -> Parser Passport
addPid b = pass >>= \p -> return $ p {pidP = Just b}

cid :: Parser String
cid = do
  symbol "cid:"
  label

addCid :: String -> Parser Passport
addCid b = pass >>= \p -> return $ p {cidP = Just b}


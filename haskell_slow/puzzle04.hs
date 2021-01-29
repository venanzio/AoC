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
      ps1 = filter valid1 ps
      ps2 = filter valid2 ps1
  putStrLn ("Number of entries: " ++ show (length ps))
  putStrLn ("Part 1: " ++ show (length ps1))
  putStrLn ("Part 2: " ++ show (length ps2))

-- Representation of passports as a record type

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


-- Part 1

valid1 :: Passport -> Bool
valid1 p = byrP p /= Nothing &&
           iyrP p /= Nothing &&
           eyrP p /= Nothing &&
           hgtP p /= Nothing &&
           hclP p /= Nothing &&
           eclP p /= Nothing &&
           pidP p /= Nothing 
  
countValid1 :: [Passport] -> Int
countValid1 = length . (filter valid1)



-- part 2

valid2 :: Passport -> Bool
valid2 p = validByr p && validIyr p && validEyr p && validHgt p &&
           validHcl p && validEcl p && validPid p



maybeParse :: Parser a -> String -> Maybe a
maybeParse pa s = case parse pa s of
  [(a,"")] -> Just a
  _        -> Nothing

countValid2 :: [Passport] -> Int
countValid2 = length . (filter valid2)





inRange :: Int -> (Int,Int) -> Bool
inRange x (low,high) = low <= x && x <= high

validYear :: Int -> Int -> String -> Bool
validYear low high s =
  length s == 4 && (all isDigit s) && inRange (read s) (low,high)





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
pass = (addField "byr:" chByr) <|>
       (addField "iyr:" chIyr) <|>
       (addField "eyr:" chEyr) <|>
       (addField "hgt:" chHgt) <|>
       (addField "hcl:" chHcl) <|>
       (addField "ecl:" chEcl) <|>
       (addField "pid:" chPid) <|>
       (addField "cid:" chCid) <|>
       (space >> return emptyPass)

pField :: String -> Parser String
pField fname = symbol fname >> label

addField :: String -> (String -> Passport -> Passport) -> Parser Passport
addField fname field = do
  fvalue <- pField fname
  p <- pass
  return (field fvalue p)

-- Functions to change passport fields
-- They all have type String -> Passport -> Passport
chByr v pass = pass {byrP = Just v}
chIyr v pass = pass {iyrP = Just v}
chEyr v pass = pass {eyrP = Just v}
chHgt v pass = pass {hgtP = Just v}
chHcl v pass = pass {hclP = Just v}
chEcl v pass = pass {eclP = Just v}
chPid v pass = pass {pidP = Just v}
chCid v pass = pass {cidP = Just v}

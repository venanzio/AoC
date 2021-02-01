-- Advent of Code 2020, day 4

module Main where

import System.Environment
import Data.Char
import Control.Applicative

import FunParser

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

-- Part 1

valid1 :: Passport -> Bool
valid1 p = byrP p /= Nothing &&
           iyrP p /= Nothing &&
           eyrP p /= Nothing &&
           hgtP p /= Nothing &&
           hclP p /= Nothing &&
           eclP p /= Nothing &&
           pidP p /= Nothing 
  
-- Part 2

valid2 :: Passport -> Bool
valid2 p = checkByr p && checkIyr p && checkEyr p && checkHgt p &&
           checkHcl p && checkEcl p && checkPid p

-- checking fields: all have type Passport -> Bool
checkByr = checkMaybe (validYear 1920 2002) . byrP
checkIyr = checkMaybe (validYear 2010 2020) . iyrP
checkEyr = checkMaybe (validYear 2020 2030) . eyrP
checkHgt = checkMaybe validHgt . hgtP
checkHcl = checkMaybe validHcl . hclP
checkEcl = checkMaybe validEcl . eclP
checkPid = checkMaybe validPid . pidP

checkMaybe :: (a -> Bool) -> Maybe a -> Bool
checkMaybe p Nothing = False
checkMaybe p (Just x ) = p x

inRange :: Int -> (Int,Int) -> Bool
inRange x (low,high) = low <= x && x <= high

validYear :: Int -> Int -> String -> Bool
validYear low high s =
  length s == 4 && (all isDigit s) && inRange (read s) (low,high)

validHgt :: String -> Bool
validHgt s = let (ds,u) = span isDigit s
                 v = read ds 
             in case u of
                  "cm" -> 150 <= v && v <= 293
                  "in" -> 59 <= v && v <= 76
                  _    -> False
                     
validHcl :: String -> Bool
validHcl s = length s == 7 &&
             head s == '#' &&
             all (`elem` "0123456789abcdef") (tail s)

validEcl :: String -> Bool
validEcl = (`elem` ["amb","blu","brn","gry","grn","hzl","oth"])

validPid :: String -> Bool
validPid s = length s == 9 && all isDigit s



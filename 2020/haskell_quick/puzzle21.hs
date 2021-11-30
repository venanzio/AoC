module PuzzleInput where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M


puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let foods = parseAll (some food) input
      alls = allAlls foods
      sings = safeIngs foods
      mps = matchPairs $ match foods 
  putStrLn (concat $ intersperse "," (map snd mps))
  return ()

type Ingredient = String
type Allergen = String
type Food = ([Ingredient],[Allergen])

food :: Parser Food
food = do
  ings <- some identifier
  symbol "(contains"
  alls <- seqSep1 identifier ","
  symbol ")"
  return (ings,alls)

-- Part 1

intersection :: Eq a => [[a]] -> [a]
intersection = foldl1 intersect

unite :: Eq a => [[a]] -> [a]
unite = foldl union []

allIngs :: [Food] -> [Ingredient]
allIngs = unite . map fst

allAlls :: [Food] -> [Allergen]
allAlls = unite . map snd

allIng :: [Food] -> Allergen -> [Ingredient]
allIng fs al = intersection [ings | (ings,alls) <- fs, al `elem` alls]

safeIngs :: [Food] -> [Ingredient]
safeIngs fs = let ings = allIngs fs
                  alls = allAlls fs
                  notSafe = unite $ map (allIng fs) alls
              in (ings \\ notSafe)

countEl :: Eq a => a -> [a] -> Int
countEl x = length . filter (x==)

countIng :: Ingredient -> [Food] -> Int
countIng ing = sum . map (countEl ing . fst)

countEls :: Eq a => [a] -> [a] -> Int
countEls xs ys = sum $ map (flip countEl ys) xs

-- Part 2

type AllMatch = [(Allergen,[Ingredient])]

initMatch :: [Food] -> AllMatch
initMatch fs = let als = allAlls fs
               in  [(al,allIng fs al) | al <- als]

sortByIngs :: AllMatch -> AllMatch
sortByIngs = sortOn (length . snd)

delIngs :: [Ingredient] -> AllMatch -> AllMatch
delIngs ings ms = map (\(a',ings') -> (a',ings' \\ ings)) ms

matchFilter :: AllMatch -> AllMatch
matchFilter [] = []
matchFilter ((a,ings):ms) =
  (a,ings) : (matchFilter $ sortByIngs (delIngs ings ms))


match :: [Food] -> AllMatch
match fs = let im = initMatch fs
           in matchFilter $ sortOn (length . snd) im

matchPairs :: AllMatch -> [(Allergen,Ingredient)]
matchPairs ms = sortOn fst [(al,ing) | (al,[ing]) <- ms]

-- Advent of Code 2022, day 5
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Data.Char

import Control.Applicative
import qualified Data.Map as M

import FunParser
import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ part1 xs)
  putStrLn ("Part 2: " ++ part2 xs)

-- Parsing the input

-- stkNum = 9

type CrateLine = [Maybe Char]
type Move = (Int,Int,Int)
type CrateStacks = [[Char]]

stkNum :: [CrateLine] -> Int
stkNum (l:_) = length l

createStacks :: [CrateLine] -> CrateStacks
createStacks cls = foldr lineStack (take (stkNum cls) (repeat [])) cls

crate :: Parser (Maybe Char)
crate = do string "   "
           return Nothing
        <|>
        do char '['
           x <- item
           char ']'
           return (Just x)

crLine :: Parser CrateLine
crLine = do xs <- manySep crate (char ' ')
            emptyLn
            return xs

move :: Parser Move
move = do symbol "move"
          n <- natural
          symbol "from"
          fr <- natural
          symbol "to"
          to <- natural
          return (n,fr,to)

pInput :: Parser (CrateStacks,[Move])
pInput = do crates <- some crLine
            line
            line
            moves <- some move
            return (createStacks crates,moves)

-- Part 1


lineStack :: CrateLine -> CrateStacks -> CrateStacks
lineStack (Nothing:crs) (st:sts) = st:lineStack crs sts
lineStack (Just x:crs)  (st:sts) = (x:st):lineStack crs sts
lineStack [] [] = []

mv :: CrateStacks -> Move -> CrateStacks
mv crs (k,n,m) =
  let xs = crs!!(n-1)
      (xs1,xs2) = splitAt k xs
      ys = crs!!(m-1)
  in replace (n-1) xs2 (replace (m-1) (reverse xs1 ++ ys) crs)

moves :: [Move] -> CrateStacks -> CrateStacks
moves mvs sts = foldl mv sts mvs

stackTops :: CrateStacks -> String
stackTops [] = ""
stackTops ([]:crs) = stackTops crs
stackTops ((c:_):crs) = c:stackTops crs

part1 :: (CrateStacks,[Move]) -> String
part1 (crs,mvs) = stackTops (moves mvs crs)

-- Part 2

mv2 :: CrateStacks -> Move -> CrateStacks
mv2 crs (k,n,m) =
  let xs = crs!!(n-1)
      (xs1,xs2) = splitAt k xs
      ys = crs!!(m-1)
  in replace (n-1) xs2 (replace (m-1) (xs1 ++ ys) crs)

moves2 :: [Move] -> CrateStacks -> CrateStacks
moves2 mvs sts = foldl mv2 sts mvs

part2 :: (CrateStacks,[Move]) -> String
part2 (crs,mvs) = stackTops (moves2 mvs crs)

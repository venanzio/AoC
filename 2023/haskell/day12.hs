-- Advent of Code 2023, day 12

--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import qualified Data.Map as M
import FunParser

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

pData :: Parser (String,[Int])
pData = do row <- token label
           groups <- someSep natural (char ',')
           return (row,groups)

pInput :: Parser [(String,[Int])]
pInput = pLines pData

-- Part 1

groupStart :: String -> [String]
groupStart r = let (ra,rb) = break (=='#') (dropWhile (=='.') r) in
  [ra'++rb | ra' <- dotGroups ra]

dotGroups :: String -> [String]
dotGroups r = let (ra,rb) = break (=='?') r in
  if rb=="" then [rb] else rb : dotGroups (tail rb)

takeGroup :: Int -> String -> [String]
takeGroup g r = let (ra,rb) = splitAt g r in
  if length ra == g && all (`elem` "#?") ra
  then if rb == "" then [""] else
        if head rb `elem` ".?" then [dropWhile (=='.') (tail rb)]
          else []
  else []

arrangements :: String -> [Int] -> Int
arrangements r gs =
  arrMap M.! (r,gs) where
  arrMap = M.fromList [((r0,gs0),arr r0 gs0) | r0 <- tails r, gs0 <- tails gs]
  arr r' [] = if all (`elem` ".?") r' then 1 else 0
  arr r' gs' | length r' < sum gs' + length gs' - 1 = 0
  arr r' (g:gs') = sum $
    [arrMap M.! (r1,gs') | r0 <- groupStart r', r1 <- takeGroup g r0]

part1 :: [(String,[Int])] -> Int
part1 xs = sum [arrangements r gs | (r,gs) <- xs]

-- Part 2

unfold :: (String,[Int]) ->  (String,[Int])
unfold (r,gs) = (concat (r:take 4 (repeat ('?':r))), concat (take 5 (repeat gs)))

part2 :: [(String,[Int])] -> Int
part2 xs = sum [arrangements r gs | (r,gs) <- map unfold xs]

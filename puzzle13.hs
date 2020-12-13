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
  let (time,busIDs) = parseAll timeBusses input
      busses = filter (>0) busIDs
      {-
      busTimes = map (busTime time) busses
      minT = minimum busTimes
      Just bt = elemIndex minT busTimes
      bus = busses !! bt
      wait = minT - time
      -}
      tids = timeID busIDs
  -- return (wait*bus)
  return (commTime tids)


timeBusses :: Parser (Int,[Int])
timeBusses = do
  time <- natural
  busses <- seqSep bus ","
  return (time,busses)
  where bus = (symbol "x" >> return (-1)) <|> natural

-- part 1

busTime :: Int -> Int -> Int
busTime t b = head $ filter (>=t) $ map (b*) [0..]

-- part 2


timeID :: [Int] -> [(Int,Int)]
timeID bs = filter (\(i,b) -> b>0) (zip [0..] bs)

comm :: [Int] -> [Int] -> [Int]
comm (x:xs) (y:ys)
  | x < y     = comm xs (y:ys)
  | x > y     = comm (x:xs) ys
  | otherwise = x: comm xs ys


commTD :: (Int,Int) ->  (Int,Int) -> (Int,Int)
commTD (t1,d1) (t2,d2) =
  (head $ comm [t1+n*d1 | n<-[0..]]  [t2+n*d2 | n<-[0..]], d1*d2)

commTDs :: [(Int,Int)] -> (Int,Int)
commTDs [td] = td
commTDs tds = let (tds1,tds2) = splitAt (length tds `div` 2) tds
              in commTD (commTDs tds1) (commTDs tds2)

commTime :: [(Int,Int)] -> Int
commTime = fst . commTDs . map (\(i,b) -> (b-i,b))

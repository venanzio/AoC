module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

input = "135468729"

-- Part 1

puzzle1 :: String -> String
puzzle1 s = let w = readC s
                w' = movesC w 100
            in final w'

-- The element of a circle contain the next element on the circle
data Circle = Circle { nextC :: M.Map Int Int,
                       currentC :: Int,
                       maxC :: Int
                     }
instance Show Circle where
  show c = show $ take (maxC c) $  fromC c (currentC c)

fromC :: Circle -> Int -> [Int]
fromC c i = i : fM c i (nxt c i)
  where fM :: Circle -> Int -> Int -> [Int]
        fM c i j = if i==j then [] else j : fM c i (nxt c j)


listM :: [Int] -> M.Map Int Int
listM (x:xs) = lM M.empty x xs 
  where lM m y [] = M.insert y x m
        lM m y (z:zs) = lM (M.insert y z m) z zs
        
listC :: [Int] -> Circle
listC xs = Circle { nextC = listM xs,
                    currentC = head xs,
                    maxC = maximum xs }


readC :: String -> Circle
readC = listC . readL

readL :: String -> [Int]
readL = parseAll rW
  where rW = some digit >>= return . map digInt
        digInt c = read [c]


nxt :: Circle -> Int -> Int
nxt c n = case M.lookup n (nextC c) of
  Nothing -> error "no next element"
  Just m  -> m

take3 :: Circle -> ([Int],Circle)
take3 c = let curr = currentC c
              x0 = nxt c curr
              x1 = nxt c x0
              x2 = nxt c x1
          in ([x0,x1,x2], c {nextC = M.insert curr (nxt c x2) (nextC c)})

destinationC :: Circle -> [Int] -> Int
destinationC c ys = dest (currentC c)
  where mx = maxC c
        dest x =
          let x' = if x-1==0 then mx else x-1
          in if x' `elem` ys then dest x' else x' 

insertC :: Int -> [Int] -> Circle -> Circle
insertC x ys c = c { nextC = insBetween x ys (nxt c x) (nextC c) }

insBetween :: Int -> [Int] -> Int -> M.Map Int Int -> M.Map Int Int
insBetween x [] z m = M.insert x z m
insBetween x (y:ys) z m = insBetween y ys z (M.insert x y m)

moveC :: Circle -> Circle
moveC c = let (picks,c0) = take3 c
              d = destinationC c0 picks
              c1 = insertC d picks c0
          in c1 {currentC = nxt c1 (currentC c1)} 

movesC :: Circle -> Int -> Circle
movesC w 0 = w
movesC w n = movesC (moveC w) (n-1)


final :: Circle -> String
final c = concat $ map show $ (tail $ fromC c 1)

-- Part 2

puzzle2 :: String -> (Int,Int)
puzzle2 s = let c = cups s 1000000
                c' = movesC c 10000000
            in resultC c'
            

cups :: String -> Int -> Circle
cups s mx =
  let l = readL s
  in listC (l ++ [(maximum l + 1) .. mx])

resultC :: Circle -> (Int,Int)
resultC c = let x1 = nxt c 1
                x2 = nxt c x1
            in (x1,x2)

main :: IO ()
main = do
  args <- getArgs
  let (x,y) = puzzle2 (head args)
  putStrLn ("first = " ++ show x ++", second = " ++ show y ++
            "; product = " ++ show (x*y))
  return ()



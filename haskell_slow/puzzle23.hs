module Main where

import System.Environment

import qualified Data.Map as M

input = "135468729"

readL :: String -> [Int]
readL = map (read.pure)

{- An abstract class to represent cycles of numbers:
     A cycle of integers is a finite map in which
     every elemnet is mapped to its successor

   We assume that elements that are not explicitely
   set with mapC have as nextC their successor
   (or 1 when they're equal to maxC)
-}

class Cycle c where
  currentC :: c -> Int   -- current element in the cycle (head)
  setCurrentC :: Int -> c -> c
  maxC     :: c -> Int   -- maximum element in the cycle
  setMaxC  :: Int -> c -> c
  nextC    :: Int -> c -> Int  -- following element in the cycle
  mapC     :: Int -> Int -> c -> c  -- inserting/changing a value
  singleC  :: Int -> c   -- singleton cycle

-- Printing a cycle, starting with the current element
showC :: Cycle c => c -> String
showC c = show $ take (maxC c) $  fromC c (currentC c)

fromC :: Cycle c => c -> Int -> [Int]
fromC c i = i : fC (nextC i c)
  where fC j = if j==i then [] else j : fC (nextC j c)

-- Cycle given by a list of integers
listC :: Cycle c => [Int] -> c
listC (x:xs) = lC x xs $ setMaxC (maximum (x:xs)) (singleC x)
  where lC y [] = mapC y x
        lC y (z:zs) = lC z zs . mapC y z


-- For part 2: completing a given list with increasing steps up to a maximum
--  we leave undefined the elements that point to the successor

cups :: Cycle c => [Int] -> Int -> c
cups xs mx = setCurrentC x $ mapC z zn $ mapC mx x $ setMaxC mx $ listC xs
  where x = head xs
        z = last xs
        zn = maximum xs + 1





{- instantiation as finite maps -}

data MapCycle = MapCycle {
                     nextMC :: M.Map Int Int,
                     currentMC :: Int,
                     maxMC :: Int
                   }

-- The next element in a cycle, if not present default to successor
nxt :: MapCycle -> Int -> Int
nxt c n = case M.lookup n (nextMC c) of
  Nothing -> n+1
  Just m  -> m

instance Cycle MapCycle where
  currentC = currentMC
  setCurrentC x c = c {currentMC = x}
  maxC     = maxMC
  setMaxC x c = c {maxMC = x}
  nextC    = flip nxt
  mapC i j c = c { nextMC = M.insert i j (nextMC c) }
  singleC i = MapCycle { nextMC = M.insert i i M.empty, currentMC = i, maxMC = i }

instance Show MapCycle where
  show = showC

instance Read MapCycle where
  readsPrec d src = [(listC (readL src), "")]


-- Implementation of the moves

-- Taking out the n elements clockwise from the current
--  The elements are left hanging in the map (will be replaced later)
takeC :: MapCycle -> Int -> ([Int],MapCycle)
takeC c n = let x0 = currentMC c
                x = nxt c x0
                l = takeM c x n
                y = nxt c (last l)
                m = nextMC c
            in (l, c { nextMC = M.insert x0 y (nextMC c) })

takeM :: MapCycle -> Int -> Int -> [Int]
takeM c x 0 = []
takeM c x n = x : takeM c (nxt c x) (n-1)

destinationC :: MapCycle -> [Int] -> Int
destinationC c ys = dest (currentMC c)
  where mx = maxMC c
        dest x =
          let x' = if x-1==0 then mx else x-1
          in if x' `elem` ys then dest x' else x' 

-- insert the elements ys after x in the cycle
insertC :: Int -> [Int] -> MapCycle -> MapCycle
insertC x ys c = c { nextMC = insBetween x ys (nxt c x) (nextMC c) }

-- insert ys between x and z:
-- x will point at the first of ys, last of ys will point at z
insBetween :: Int -> [Int] -> Int -> M.Map Int Int -> M.Map Int Int
insBetween x [] z m = M.insert x z m
insBetween x (y:ys) z m = insBetween y ys z (M.insert x y m)

-- One move: pick up three cups, insert them after the destination,
--           move the current cup one place clockwise
moveC :: MapCycle -> MapCycle
moveC c = let (picks,c0) = takeC c 3
              d = destinationC c0 picks
              c1 = insertC d picks c0
          in c1 {currentMC = nxt c1 (currentMC c1)} 

-- Performing n moves in sequence
movesC :: MapCycle -> Int -> MapCycle
movesC c 0 = c
movesC c n = movesC (moveC c) (n-1)





-- Part 1

puzzle1 :: String -> String
puzzle1 s = let w = read s
                w' = movesC w 100
            in final w'

final :: MapCycle -> String
final c = concat $ map show $ (tail $ fromC c 1)

-- Part 2

puzzle2 :: String -> (Int,Int)
puzzle2 s = let c = cups (readL s) 1000000
                c' = movesC c 10000000
            in resultC c'
            


resultC :: MapCycle -> (Int,Int)
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



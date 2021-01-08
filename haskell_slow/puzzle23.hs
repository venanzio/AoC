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

-- move the current element one step clockwise
rightC :: Cycle c => c -> c
rightC c = setCurrentC (nextC (currentC c) c) c

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

-- Implementation of the moves

-- Taking out the n elements clockwise from the current
--  The elements are left hanging in the cycle (will be replaced later)
takeC :: Cycle c => c -> Int -> ([Int],c)
takeC c n = let x0 = currentC c
                x = nextC x0 c
                l = takeL x n c
                y = nextC (last l) c
            in (l, mapC x0 y c)

-- take the list of n elements starting from x clockwise
takeL :: Cycle c => Int -> Int -> c -> [Int]
takeL x 0 c = []
takeL x n c = x : takeL (nextC x c) (n-1) c

-- destination: current - 1, avoiding the elements in the list
destinationC :: Cycle c => [Int] -> c -> Int
destinationC ys c = dest (currentC c)
  where mx = maxC c
        dest x =
          let x' = if x-1==0 then mx else x-1
          in if x' `elem` ys then dest x' else x' 

-- insert the elements ys after x in the cycle
insertC :: Cycle c => Int -> [Int] -> c -> c
insertC x ys c = insBetween x ys (nextC x c) c

-- insert ys between x and z:
-- x will point at the first of ys, the last of ys will point at z
insBetween :: Cycle c => Int -> [Int] -> Int -> c -> c
insBetween x [] z = mapC x z
insBetween x (y:ys) z = insBetween y ys z . mapC x y

-- One move: pick up three cups, insert them after the destination,
--           move the current cup one place clockwise
moveC :: Cycle c => c -> c
moveC c = let (picks,c0) = takeC c 3
              d = destinationC picks c0
              c1 = insertC d picks c0
          in rightC c1

-- Performing n moves in sequence
movesC :: Cycle c => Int -> c -> c
movesC 0 = id
movesC n = movesC (n-1) . moveC

-- Part 1

puzzle1 :: String -> String
puzzle1 s = let c = read s :: MapCycle 
                c' = movesC 100 c
            in final c'

final :: MapCycle -> String
final c = concat $ map show $ (tail $ fromC c 1)

-- Part 2

puzzle2 :: String -> (Int,Int)
puzzle2 s = let c = cups (readL s) 1000000
                c' = movesC 10000000 c
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

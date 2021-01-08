module Main where

import System.Environment

import qualified Data.Map as M

-- import qualified Data.Array as A
import qualified GHC.Arr as A

import qualified Data.Array.IO as Arr
import System.IO.Unsafe

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
  nextC    :: Int -> c -> Int  -- following element in the cycle
  mapC     :: Int -> Int -> c -> c  -- inserting/changing a value
  defaultC :: Int -> c -- cycle with given maximum, default values (successors)

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
listC xs = listMaxC (maximum xs) xs

-- Cycle from list and given maximum (elements not in list default)
listMaxC :: Cycle c => Int -> [Int] -> c
listMaxC mx (x:xs) = lC x xs $ defaultC mx  -- setMaxC (maximum (x:xs)) (singleC x)
  where lC y [] = mapC y x
        lC y (z:zs) = lC z zs . mapC y z

-- For part 2: completing a given list with increasing steps up to a maximum
--  we leave undefined the elements that point to the successor

cups :: Cycle c => [Int] -> Int -> c
cups xs mx = setCurrentC x $ mapC z zn $ mapC mx x $ listMaxC mx xs
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
puzzle1 s = let c = listC $ readL s :: ArrCycle -- change to any instance of Cycle
                c' = movesC 100 c
            in final c'

final :: Cycle c => c -> String
final c = concat $ map show $ (tail $ fromC c 1)

-- Part 2

-- Unfortunately this works less well than the quick version
--   (Overhead due to abstraction?)

puzzle2 :: String -> (Int,Int)
puzzle2 s = let c = cups (readL s) 1000000 :: ArrCycle
                c' = movesC 10000000 c
            in resultC c'

resultC :: Cycle c => c -> (Int,Int)
resultC c = let x1 = nextC 1 c
                x2 = nextC x1 c
            in (x1,x2)

main :: IO ()
main = do
  args <- getArgs
  let (x,y) = puzzle2 (head args)
  putStrLn ("first = " ++ show x ++", second = " ++ show y ++
            "; product = " ++ show (x*y))
  return ()


-- Instantiation as finite maps

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
  nextC    = flip nxt
  mapC i j c = c { nextMC = M.insert i j (nextMC c) }
  defaultC mx = MapCycle { nextMC = M.insert mx 1 M.empty, currentMC = 1, maxMC = mx }

instance Show MapCycle where
  show = showC

instance Read MapCycle where
  readsPrec d src = [(listC (readL src), "")]


-- Instantiation as arrays

data ACycle = AC Int (A.Array Int Int)

instance Cycle ACycle where
  currentC (AC x _) = x
  setCurrentC x (AC y a) = (AC x a)
  maxC (AC y a) = snd (A.bounds a)
  nextC x (AC _ a) = a A.! x
  mapC i j (AC x a) = AC x (a A.// [(i,j)])
  defaultC mx = AC 1 (A.array (1,mx) ([(i,i+1) | i <- [1..mx-1]] ++ [(mx,1)]))

-- Instantiation as mutable arrays (with unsafe operations)
--  DOESN'T WORK : needs to be done propertly without unsafe tricks

data ArrCycle = ArrC Int (Arr.IOArray Int Int)

instance Cycle ArrCycle where
  currentC (ArrC x _) = x
  setCurrentC x (ArrC y a) = (ArrC x a)
  maxC (ArrC y a) = snd (unsafePerformIO $ Arr.getBounds a)
  nextC x (ArrC _ a) = unsafePerformIO $ Arr.readArray a x
  mapC i j (ArrC x a) = ArrC x $ unsafePerformIO (Arr.writeArray a i j >> return a)
  defaultC mx = ArrC 1 (unsafePerformIO $ Arr.newListArray (1,mx) ([2..mx]++[1]))


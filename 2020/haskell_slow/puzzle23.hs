module Main where

import System.Environment
import Control.Monad

import Data.IORef
import Data.Array.IO

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

-- A cycle is described by the number of elements (fixed),
--   the current element, and the array of next elements
type Cycle = (Int, IORef Int, IOArray Int Int)

-- Current element in the cycle (head)
current :: Cycle -> IO Int
current (_,curr,_) = readIORef curr

setCurrent :: Cycle -> Int -> IO ()
setCurrent (_,curr,_) = writeIORef curr

-- Maximum element = size of the cycle, the elements are 1,..,mx
maxC :: Cycle -> IO Int
maxC (n,_,_) = return n

-- Element following i in the cycle
next :: Cycle -> Int -> IO Int
next (_,_,a) i = readArray a i

-- Changing the element following i
link :: Cycle -> Int -> Int -> IO ()
link (_,_,a) i j = writeArray a i j

-- Default cycle with n elements, starting at 1, going through n, then back at 1
defaultC :: Int -> IO Cycle
defaultC n = do
  curr <- newIORef 1
  a <- newListArray (1,n) ([ i+1 | i <- [1..n-1]] ++ [1])
  return (n,curr,a)

printC :: Cycle -> IO ()
printC cycle = do
  l <- elemsC cycle
  putStrLn (show l)

elemsC :: Cycle -> IO [Int]
elemsC cycle = current cycle >>= fromC cycle 

fromC :: Cycle -> Int -> IO [Int]
fromC cycle i = do
  j <- next cycle i
  let fC j = if j==i then return []
                     else do k <- next cycle j
                             l <- fC k
                             return (j : l)
  l <- fC j
  return (i:l)

-- move the current element one step clockwise
rightC :: Cycle -> IO ()
rightC cycle = do
  c <- current cycle
  cn <- next cycle c
  setCurrent cycle cn


-- Cycle given by a list of integers
listC :: [Int] -> IO Cycle
listC xs = listMaxC (length xs) xs

-- Cycle from list and given maximum (elements not in list default)
listMaxC :: Int -> [Int] -> IO Cycle
listMaxC n [] = defaultC n
listMaxC n (x:xs) = do
  cycle <- defaultC n
  setCurrent cycle x
  last <- lMAdd cycle x xs
  let nxt = maximum (x:xs) + 1
  if nxt <= n
    then do
      link cycle last nxt
      link cycle n x    
    else link cycle last x
  return cycle

lMAdd :: Cycle -> Int -> [Int] -> IO Int
lMAdd cycle y [] = return y
lMAdd cycle y (z:zs) = link cycle y z >> lMAdd cycle z zs

-- Implementation of the moves

-- Taking out the n elements clockwise from the current
--  The elements are left hanging in the cycle (will be replaced later)
takeC :: Cycle -> Int -> IO [Int]
takeC cycle 0 = return []
takeC cycle n = do
  c <- current cycle
  x <- next cycle c
  l <- takeL cycle x n
  y <- next cycle (last l)
  link cycle c y
  return l

-- take the list of n elements starting from x clockwise
takeL :: Cycle -> Int -> Int -> IO [Int]
takeL cycle x 0 = return []
takeL cycle x n = do
  y <- next cycle x
  xs <- takeL cycle y (n-1)
  return (x:xs)

-- destination: current - 1, avoiding the elements in the list
destination :: Cycle -> [Int] -> IO Int
destination cycle ys = do
  n <- maxC cycle
  x <- current cycle
  return (destFrom ys n x)

destFrom :: [Int] -> Int -> Int -> Int
destFrom ys n x =
  let x' = if x==1 then n else x-1
  in if x' `elem` ys then destFrom ys n x' else x'

-- insert the elements ys after x in the cycle
insertC :: Cycle -> Int -> [Int] -> IO ()
insertC cycle x ys = do
  z <- next cycle x
  insBetween cycle x ys z

-- insert ys between x and z:
-- x will point at the first of ys, the last of ys will point at z
insBetween :: Cycle -> Int -> [Int] -> Int -> IO ()
insBetween cycle x [] z = link cycle x z
insBetween cycle x (y:ys) z = link cycle x y >> insBetween cycle y ys z

-- One move: pick up three cups, insert them after the destination,
--           move the current cup one place clockwise
move :: Cycle -> IO ()
move cycle = do
  picks <- takeC cycle 3
  d <- destination cycle picks
  insertC cycle d picks
  rightC cycle

-- Performing n moves in sequence
movesC :: Cycle -> Int -> IO ()
movesC cycle n = replicateM_ n (move cycle)

-- Part 1

puzzle1 :: String -> IO String
puzzle1 s = do
  cycle <- listC $ readL s
  movesC cycle 100
  final cycle

final :: Cycle -> IO String
final cycle = do
  l <- fromC cycle 1
  return (concat $ map show (tail l))

-- Part 2

puzzle2 :: String -> IO ()
puzzle2 s = do
  cycle <- listMaxC 1000000 (readL s)
  movesC cycle 10000000
  (x1,x2) <- resultC cycle
  let x = x1*x2
  putStrLn ("first = " ++ show x1 ++", second = " ++ show x2 ++
            "; product = " ++ show x)

resultC :: Cycle -> IO (Int,Int)
resultC cycle = do
  x1 <- next cycle 1
  x2 <- next cycle x1
  return (x1,x2)

main :: IO ()
main = do
  args <- getArgs
  puzzle2 (head args)

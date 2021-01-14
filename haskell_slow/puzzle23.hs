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
maxC (mx,_,_) = return mx

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

{-

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
  linkC i j c = c { nextMC = M.insert i j (nextMC c) }
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
  linkC i j (AC x a) = AC x (a A.// [(i,j)])
  defaultC mx = AC 1 (A.array (1,mx) ([(i,i+1) | i <- [1..mx-1]] ++ [(mx,1)]))

-- Instantiation as mutable arrays (with unsafe operations)
--  DOESN'T WORK : needs to be done propertly without unsafe tricks

data ArrCycle = ArrC Int (Arr.IOArray Int Int)

instance Cycle ArrCycle where
  currentC (ArrC x _) = x
  setCurrentC x (ArrC y a) = (ArrC x a)
  maxC (ArrC y a) = snd (unsafePerformIO $ Arr.getBounds a)
  nextC x (ArrC _ a) = unsafePerformIO $ Arr.readArray a x
  linkC i j (ArrC x a) = ArrC x $ unsafePerformIO (Arr.writeArray a i j >> return a)
  defaultC mx = ArrC 1 (unsafePerformIO $ Arr.newListArray (1,mx) ([2..mx]++[1]))

-}



main = putStrLn "Hello World!"

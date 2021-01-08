module Main where

import System.Environment

import qualified Data.Map as M

input = "135468729"

readL :: String -> [Int]
readL = map (read.pure)


{- A cycle of integers is a finite map in which
   every elemnet is mapped to its successor
-}

data Cycle = Cycle { nextC :: M.Map Int Int,
                     currentC :: Int,
                     maxC :: Int   -- highest value un the cycle
                   }

-- The next element in a cycle, if not present default to successor
nxt :: Cycle -> Int -> Int
nxt c n = case M.lookup n (nextC c) of
  Nothing -> n+1
  Just m  -> m

instance Show Cycle where
  show c = show $ take (maxC c) $  fromC c (currentC c)

fromC :: Cycle -> Int -> [Int]
fromC c i = i : fM c i (nxt c i)
  where fM :: Cycle -> Int -> Int -> [Int]
        fM c i j = if i==j then [] else j : fM c i (nxt c j)

instance Read Cycle where
  readsPrec d src = [(listC (readL src), "")]

listMap :: [Int] -> M.Map Int Int
listMap (x:xs) = lM M.empty x xs 
  where lM m y [] = M.insert y x m
        lM m y (z:zs) = lM (M.insert y z m) z zs
        
listC :: [Int] -> Cycle
listC xs = Cycle { nextC = listMap xs,
                   currentC = head xs,
                   maxC = maximum xs }

-- For part 2: completing a given list with increasing steps up to a maximum
--  we leave undefined the elements that point to the successor

cups :: String -> Int -> Cycle
cups s mx =
  let xs = readL s
      x = head xs
      z = last xs
      zn = maximum xs + 1
  in Cycle { nextC = M.insert z zn $ M.insert mx x (listMap xs),
             currentC = x,
             maxC = mx }

-- Implementation of the moves

-- Taking out the n elements clockwise from the current
--  The elements are left hanging in the map (will be replaced later)
takeC :: Cycle -> Int -> ([Int],Cycle)
takeC c n = let x0 = currentC c
                x = nxt c x0
                l = takeM c x n
                y = nxt c (last l)
                m = nextC c
            in (l, c { nextC = M.insert x0 y (nextC c) })

takeM :: Cycle -> Int -> Int -> [Int]
takeM c x 0 = []
takeM c x n = x : takeM c (nxt c x) (n-1)

destinationC :: Cycle -> [Int] -> Int
destinationC c ys = dest (currentC c)
  where mx = maxC c
        dest x =
          let x' = if x-1==0 then mx else x-1
          in if x' `elem` ys then dest x' else x' 

-- insert the elements ys after x in the cycle
insertC :: Int -> [Int] -> Cycle -> Cycle
insertC x ys c = c { nextC = insBetween x ys (nxt c x) (nextC c) }

-- insert ys between x and z:
-- x will point at the first of ys, last of ys will point at z
insBetween :: Int -> [Int] -> Int -> M.Map Int Int -> M.Map Int Int
insBetween x [] z m = M.insert x z m
insBetween x (y:ys) z m = insBetween y ys z (M.insert x y m)

-- One move: pick up three cups, insert them after the destination,
--           move the current cup one place clockwise
moveC :: Cycle -> Cycle
moveC c = let (picks,c0) = takeC c 3
              d = destinationC c0 picks
              c1 = insertC d picks c0
          in c1 {currentC = nxt c1 (currentC c1)} 

-- Performing n moves in sequence
movesC :: Cycle -> Int -> Cycle
movesC c 0 = c
movesC c n = movesC (moveC c) (n-1)





-- Part 1

puzzle1 :: String -> String
puzzle1 s = let w = read s
                w' = movesC w 100
            in final w'

final :: Cycle -> String
final c = concat $ map show $ (tail $ fromC c 1)

-- Part 2

puzzle2 :: String -> (Int,Int)
puzzle2 s = let c = cups s 1000000
                c' = movesC c 10000000
            in resultC c'
            


resultC :: Cycle -> (Int,Int)
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



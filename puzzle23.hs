module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

input = "135468729"


{-
puzzle2 :: String -> Int
puzzle2 s = let l = readL s ++ [10..1000000]
                w = listLW l
                w' = moves2 w 10 -- 10000000
                (x,y) = result2 w'
            in x*y
-}

readWheel :: String -> Wheel Int
readWheel = listLW . readL

{-
readL :: String -> [Int]
readL = parseAll rW
  where rW = some digit >>= return . map digInt
        digInt c = read [c]
-}

-- WHEELS

type Wheel a = ([a],[a])

readW :: Wheel a -> a
readW (x:_,_) = x
readW ([],zs) = last zs

emptyW :: Wheel a
emptyW = ([],[])

isEmptyW :: Wheel a -> Bool
isEmptyW ([],[]) = True
isEmptyW _ = False

-- Turning a list into a wheel
--   The last element of the list is linked back to the first
--   This "bends the list around clockwise"
listLW :: [a] -> Wheel a
listLW xs = (xs1, reverse xs2)
  where (xs1,xs2) = splitAt (length xs `div` 2) xs

-- Bending the list around anticlockwise
listRW :: [a] -> Wheel a
listRW xs = (reverse xs2, xs1)
  where (xs1,xs2) = splitAt (length xs `div` 2) xs

-- Turning a wheel into a list
--  just forget the link from last to first element
wheelL :: Wheel a -> [a]
wheelL (ys,zs) = ys ++ reverse zs

-- Move to the next element clockwise
rightW :: Wheel a -> Wheel a
rightW (y:ys,zs) = (ys,y:zs)
rightW ([],[]) = ([],[])
rightW ([],zs) = rightW (listRW zs)

-- Move to the next element anti-clockwise
leftW :: Wheel a -> Wheel a
leftW (ys,z:zs) = (z:ys,zs)
leftW ([],[]) = ([],[])
leftW (ys,[]) = leftW (listLW ys)

-- insert a new element as head
insertW :: a -> Wheel a -> Wheel a
insertW x (ys,zs) = (x:ys,zs)

-- extract and delete the head
-- move the head to the next right
extractW :: Wheel a -> (a, Wheel a)
extractW (y:ys,zs) = (y,(ys,zs))
extractW ([],[]) = error "empty wheel"
extractW ([],zs) = extractW (listRW zs)

deleteW :: Wheel a -> Wheel a
deleteW = snd.extractW

-- Concatenate two wheels
--   The new head is the head of the first (if non-empty)
--   (complexity: can we make it O(1)?)
concatW :: Wheel a -> Wheel a -> Wheel a
concatW (ys1,zs1) (ys2,zs2) = (ys1 ++ reverse zs1, zs2 ++ reverse ys2)

-- Part 1

getThree :: Wheel Int -> ([Int],Wheel Int)
getThree w = let w0 = rightW w
                 (x1,w1) = extractW w0
                 (x2,w2) = extractW w1
                 (x3,w3) = extractW w2
             in ([x1,x2,x3],leftW w3)

findW :: Int -> Wheel Int -> Wheel Int
findW x w =
  if readW w == x then w else findW x (rightW w)

destination :: Int -> [Int] -> Int
destination x ys = if x' `elem` ys then destination x' ys else x'  
  where x' = if x-1==0 then 9 else x-1

insertR :: [Int] -> Wheel Int -> Wheel Int
insertR xs w = let w0 = rightW w
                   w1 = foldr insertW w0 xs
               in leftW w1

move :: Wheel Int -> Wheel Int
move w = let current = readW w
             (picks,w0) = getThree w
             d = destination current picks
             w1 = findW d w0
             w2 = insertR picks w1 
         in rightW (findW current w2)

moves :: Wheel Int -> Int -> Wheel Int
moves w 0 = w
moves w n = moves (move w) (n-1)

result :: Wheel Int -> String
result w = concat $ map show $ (tail $ wheelL $ findW 1 w)








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


insertC :: Int -> [Int] -> Circle -> Circle
insertC x ys c = c { nextC = insBetween x ys (nxt c x) (nextC c) }

insBetween :: Int -> [Int] -> Int -> M.Map Int Int -> M.Map Int Int
insBetween x [] z m = M.insert x z m
insBetween x (y:ys) z m = insBetween y ys z (M.insert x y m)

moveC :: Circle -> Circle
moveC c = let (picks,c0) = take3 c
              d = destination (currentC c0) picks
              c1 = insertC d picks c0
          in c1 {currentC = nxt c1 (currentC c1)} 

movesC :: Circle -> Int -> Circle
movesC w 0 = w
movesC w n = movesC (moveC w) (n-1)


final :: Circle -> String
final c = concat $ map show $ (tail $ fromC c 1)




-- Part 2

puzzle2 :: String -> (Int,Int)
puzzle2 s = let c = cups s
                c' = movesC c 10000000
            in resultC c'
            

cups :: String -> Circle
cups s = let l = readL s
         in listC (l ++ [(maximum l + 1) .. 1000000])

resultC :: Circle -> (Int,Int)
resultC c = let x1 = nxt c 1
                x2 = nxt c x1
            in (x1,x2)

main :: IO ()
main = do
  let (x,y) = puzzle2 input
  putStrLn ("first = " ++ show x ++", second = " ++ show y ++
            "; product = " ++ show (x*y))
  return ()



{-
destination2 :: Int -> [Int] -> Int
destination2 x ys = if x' `elem` ys then destination2 x' ys else x'  
  where x' = if x-1==0 then 1000000 else x-1

move2 :: Wheel Int -> Wheel Int
move2 w = let current = readW w
              (picks,w0) = getThree w
              d = destination2 current picks
              w1 = findW d w0
              w2 = insertR picks w1 
          in rightW (findW current w2)

moves2 :: Wheel Int -> Int -> Wheel Int
moves2 w 0 = w
moves2 w n = moves2 (move2 w) (n-1)

result2 :: Wheel Int -> (Int,Int)
result2 w = let w1 = rightW $ findW 1 w
                (x,w2) = extractW w1
                (y,_) = extractW w2
         in (x,y)
-}

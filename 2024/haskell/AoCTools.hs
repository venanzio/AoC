-- Advent of Code: Useful Functions
--  Venanzio Capretta 2020 - 2023

module AoCTools where

import Data.List
import Data.Function (on)
import qualified Data.Map as M

-- LISTS

{- ALREADY EXISTS: partition
spanBy :: (a->Bool) -> [a] -> ([a],[a])
spanBy p [] = ([],[])
spanBy p (x:xs) = let (ys,zs) = spanBy p xs
                  in if p x then (x:ys,zs)
                            else (ys,x:zs)
-}

-- total tail: returns [] on []
tailT :: [a] -> [a]
tailT [] = []
tailT xs = tail xs

-- number of occurrences of an element in a list
occurrences :: Eq a => a -> [a] -> Int
occurrences x ys = length (filter (== x) ys)

-- Replace an element of a list (at given index)
replace :: Int -> a -> [a] -> [a]
replace i x l =
  let (front,back) = splitAt i l
  in front ++ x:(tail back)

-- apply a function to an element of a list
replaceF :: Int -> (a -> a) -> [a] -> [a]
replaceF i f l =
  let (front,back) = splitAt i l
  in front ++ (f (head back)):(tail back)

-- indices of occurrences of an element
allIndices :: Eq a => a -> [a] -> [Int]
allIndices x ys = case elemIndex x ys of
  Nothing -> []
  Just i  -> i : map (+(i+1)) (allIndices x (drop (i+1) ys))

-- all deletions of one element
delOne :: [a] -> [[a]]
delOne = delOneAux [] where
  delOneAux xs [] = []
  delOneAux xs (y:ys) = (xs++ys) : delOneAux (xs++[y]) ys

-- delete element with index i
delIndex :: Int -> [a] -> [a]
delIndex i xs = (delOne xs)!!i


-- delete all occurrences
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll x ys = case elemIndex x ys of
    Nothing -> ys
    Just i -> let (ys0,ys1) = splitAt i ys
              in ys0 ++ deleteAll x (tail ys1)

-- successive elements satisfy a relation
allRel :: (a->a->Bool) -> [a] -> Bool
allRel rel (x0:xs@(x1:_)) = rel x0 x1 && allRel rel xs
allRel _ _ = True

-- apply a binary function element-wise
zipWithLong :: (a -> a -> a) -> [a] -> [a] -> [a] 
zipWithLong f [] ys = ys
zipWithLong f xs [] = xs
zipWithLong f (x:xs) (y:ys) = f x y : zipWithLong f xs ys

-- compose (forward) a list of functions
fsCompose :: [a -> a] -> a -> a 
fsCompose [] x = x
fsCompose (f:fs) x = fsCompose fs (f x)

{- already exists: mapMaybe in Data.Maybe
findMaybe :: (a -> Maybe b) -> [a] -> [b]
findMaybe f [] = []
findMaybe f (x:xs) = let fxs = findMaybe f xs in
  case (f x) of
    Just y -> y : fxs
    Nothing -> fxs
-}

-- Minimum with a highest bound (for empty list)
minimumBound :: Ord a => a -> [a] -> a
minimumBound x = minimum . (x:)

-- greatest common divisor of a list
gcdL :: (Integral a) => [a] -> a
gcdL = foldr gcd 0

-- least common multiple of a list
lcmL :: (Integral a) => [a] -> a
lcmL = foldr lcm 1

-- repeat a list infinitely many times
ouroboros :: [a] -> [a]
ouroboros xs = xs ++ ouroboros xs

-- all non-ordered pairs of eleemnts of a list
allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = map (\y -> (x,y)) xs ++ allPairs xs


-- ORBITS OF FUNCIONS

orbitCycle :: Eq a => (a->a) -> a -> (Int,Int)
orbitCycle f x0 = let (xs,ys) = orbit f x0 in (length xs, length ys)

-- iterate a function until it enters a repeating loop
-- returns the values before the loop and the loop itself
orbit :: Eq a => (a->a) -> a -> ([a],[a])
orbit f x0 = listOrbit (iterate f x0)

-- find the first repetition of an element in a list
-- return the part of the list before the first occurrence of
-- the repeated element and the part from that element to the second occurrence excluded
listOrbit :: Eq a => [a] -> ([a],[a])
listOrbit xs = orb [] xs where
  orb vs [] = (vs,[])
  orb vs (x:xs) = let (vs1,vs0) = break (==x) vs
                   in if vs0 == [] then orb (x:vs) xs
                      else (reverse (tail vs0), x : reverse vs1)

-- find the nth iteration of f starting at x0
-- when we know that the iterations will loop
-- (for values of n much larger that the loop)
loopf :: Eq a => (a->a) -> a -> Int -> a
loopf f x0 n =
  let (xs,ys) = orbit f x0
      lead = length xs
      loop = length ys
  in if n < lead then xs!!n else ys!!((n-lead) `rem` loop)




-- RANGES

-- An interval is a pair (x,y) denoting [x..y]
--   if y<x, the interval is empty
type Interval = (Int,Int)

iStart :: Interval -> Int
iStart = fst

iEnd :: Interval -> Int
iEnd = snd

iEmpty :: Interval -> Bool
iEmpty (x,y) = y < x

inInterval :: Int -> Interval -> Bool
inInterval v (x,y) = x<=v && v<=y

sizeInterval :: Interval -> Int
sizeInterval (x,y) =
  if iEmpty (x,y) then 0 else y-x+1

-- create anInterval given first element and length
slInterval :: Int -> Int -> Interval
slInterval s l = (s,s+l-1)

-- Intersection (may give an empty interval)
iIntersection :: Interval -> Interval -> Interval
iIntersection (x0,y0) (x1,y1) = (max x0 x1, min y0 y1)

-- A range is an ordered list of non-overlapping intervals
--   operations preserve ordering and non-overlapping property
type Range = [Interval]

emptyR :: Range
emptyR = []

-- This should never be needed, if the range is correct
neRange :: Range -> Range
neRange = filter (not.iEmpty)

sizeRange :: Range -> Int
sizeRange = sum . map sizeInterval

addInterval :: Interval -> Range -> Range
addInterval i [] = if iEmpty i then [] else [i]
addInterval i@(x,y) r@(i0@(x0,y0):r1)
  | iEmpty i = r
  | y + 1 < x0 = i : r
  | y0 + 1 < x = i0 : addInterval i r1
  | otherwise = addInterval (min x x0, max y y0) r1

-- Single interval range (just check that it is not empty)
intRange :: Interval -> Range
intRange i = addInterval i emptyR

-- Create a range from a non-ordered list of intervals
intsRange :: [Interval] -> Range
intsRange = foldr addInterval emptyR

rUnion :: Range -> Range -> Range
rUnion [] r1 = r1
rUnion r0 [] = r0
rUnion  r0@(i0@(x0,y0):r0') r1@(i1@(x1,y1):r1')
  | x0 < x1 = addInterval i0 (rUnion r0' r1)
  | otherwise = addInterval i1 (rUnion r0 r1')

rIntersection :: Range -> Range -> Range
rIntersection [] _ = []
rIntersection _ [] = []
rIntersection r0@(i0@(x0,y0):r0') r1@(i1@(x1,y1):r1')
  | y0 < x1 = rIntersection r0' r1
  | y1 < x0 = rIntersection r0 r1'
  | y0 < y1 = addInterval (iIntersection i0 i1) (rIntersection r0' r1)
  | otherwise = addInterval (iIntersection i0 i1) (rIntersection r0 r1')

rMinimum :: Range -> Int
rMinimum [] = error "empty range has no minimum"
rMinimum r = fst (r!!0)

rMaximum :: Range -> Int
rMaximum [] = error "empty range has no maximum"
rMaximum r = snd (last r)

-- Difference between two intervals
iDiff :: Interval -> Interval -> Range
iDiff (x0,y0) (x1,y1) = intsRange [(x0,min y0 (x1-1)),(max x0 (y1+1),y0)]

-- complement of a range inside a give interval
rComplement :: Interval -> Range -> Range
rComplement i [] = intsRange [i]
rComplement i@(x0,y0) ((x1,y1):r)
  | y0 < x0   = []
  | y0 < x1   = intsRange [i]
  | otherwise = addInterval (x0,x1-1) (rComplement (y1+1,y0) r)
  
-- Difference between two ranges
rDiff :: Range -> Range -> Range
rDiff r0 r1 = rIntersection r0 (rComplement (rMinimum r0, rMaximum r0) r1)






-- MAYBE operations

-- list of Just values of a Maybe element (at most one)
justL :: Maybe a -> [a]
justL (Just x) = [x]
justL Nothing = []

-- Remove Just from a Maybe valus
unJust :: Maybe a -> a
unJust (Just x) = x
unJust y = error "can't unJust Nothing"

-- and with a default value for Nothing
unJustD :: a -> Maybe a -> a
unJustD d Nothing = d
unJustD _ (Just x) = x

-- Iterating a function n times
nIter :: (a->a) -> Int -> a->a
nIter f 0 x = x
nIter f n x = nIter f (n-1) (f x)

filterIndices :: (a -> Bool) -> [a] -> [(Int,a)]
filterIndices p xs = fIndAux 0 xs where
  fIndAux n [] = []
  fIndAux n (x:xs) = if p x then (n,x) : fIndAux (n+1) xs else fIndAux (n+1) xs

-- Keeping the Just values from a list of Maybe
filterJust :: [Maybe a] -> [a]
filterJust l = [x | Just x <- l]










-- Minimize a function over a list and return: index, element, function value
minimumF :: Ord b => (a->b) -> [a] -> (Int,a,b)
minimumF f (x:xs) =
  foldl (\(i,y,v) (j,z) -> let w = f z in
                           if w<v then (j,z,w) else (i,y,v))
        (0,x,f x) (zip [1..] xs)

-- Maximize a function over a list and return: index, element, function value
maximumF :: Ord b => (a->b) -> [a] -> (Int,a,b)
maximumF f [] = error "Can't compute the maximum of an empty list"
maximumF f (x:xs) =
  foldl (\(i,y,v) (j,z) -> let w = f z in
                           if w>v then (j,z,w) else (i,y,v))
        (0,x,f x) (zip [1..] xs)

maxF :: Ord b => (a->b) -> [a] -> (a,b)
maxF f xs = let (_,e,max) = maximumF f xs in (e,max)

-- Index and value of maximum
imax :: Ord a => [a] -> (Int,a)
imax xs = let (i,m,_) = maximumF id xs in (i,m)

-- mapping with indices: f a function of index and value
imap :: (Int -> a -> b) -> [a] -> [b]
imap f = imap_aux 0 where
  imap_aux i [] = []
  imap_aux i (x:xs) = f i x : imap_aux (i+1) xs


-- INDEXED MAPS

type Point = (Int,Int)

-- list to index map with indices as keys, starting at index i0
listMap :: Int -> [a] -> M.Map Int a
listMap i0 = M.fromAscList . (zip [i0..])

lMap :: [a] -> M.Map Int a
lMap = listMap 0

-- 2-dimentional matrix to index map, with coordinates as keys
matrixMap :: Point -> [[a]] -> M.Map Point a
matrixMap (i0,j0) xss = M.fromList [((i0+i,j0+j), xss!!j!!i) |
                                    j <- [0 .. length xss - 1],
                                    i <- [0 .. length (xss!!j) - 1]]
                        
mMap :: [[a]] -> M.Map Point a
mMap = matrixMap (0,0)

matrixMapF :: Point -> (a -> Maybe b) -> [[a]] -> M.Map Point b
matrixMapF (i0,j0) f xss =
  M.fromList [((i0+i,j0+j), b)
             | j <- [0 .. length xss - 1]
             , i <- [0 .. length (xss!!j) - 1]
             , b <- justL (f (xss!!j!!i))
             ]
mMapF :: (a -> Maybe b) -> [[a]] -> M.Map Point b
mMapF = matrixMapF (0,0)

matrixMapFP :: Point -> (Point -> a -> Maybe b) -> [[a]] -> M.Map Point b
matrixMapFP (i0,j0) f xss =
  M.fromList [((i0+i,j0+j), b)
             | j <- [0 .. length xss - 1]
             , i <- [0 .. length (xss!!j) - 1]
             , b <- justL (f (i0+i,j0+j) (xss!!j!!i))
             ]
mMapFP :: (Point -> a -> Maybe b) -> [[a]] -> M.Map Point b
mMapFP = matrixMapFP (0,0)




fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y

divide :: Eq a => a -> [a] -> [[a]]
divide _ [] = []
divide x l = case break (==x) l of
  (xs,[]) -> [xs]
  (xs,(_:ys)) -> xs : divide x ys

maximumB :: Ord a => a -> [a] -> a
maximumB x xs = maximum (x:xs)





infinite = maxBound `div` 2 :: Int

minimumInf :: [Int] -> Int
minimumInf xs = minimum (infinite:xs)

type Graph a = M.Map a [(a,Int)]

type Queue a = M.Map a Int

relax :: Ord a => Graph a -> Queue a -> a -> Int -> Queue a
relax graph queue x dx =
  foldl (\q (y,dxy) -> M.adjust (\dy -> min dy (dx+dxy)) y q) queue (graph  M.! x)
           -- M.insertWith min y (dx+dxy) q) queue (graph  M.! x)

dijkstra :: Ord a => Graph a -> a -> a -> Int
dijkstra graph s t =
  dijkstra_aux $ M.fromList ((s,0) : [(v,infinite) | v <- M.keys graph, v /= s])
  where dijkstra_aux queue =
          let (x,dx) = minimumBy (compare `on` snd) (M.toAscList queue)
              {-
              v = graph M.! x
              relax y dy = min dy (d + dist graph x y)
              -}
          in if x==t then dx
               else dijkstra_aux (relax graph (M.delete x queue) x dx)
                    -- [(y, relax y dy) | (y,dy) <- queue, y/=x]
 
        



-- Winding number , copied from day 10

-- Winding number of a loop around a point
-- This is not quite correct
winding :: Point -> [Point] -> Int
winding (x0,y0) l = winDir 0 lastX l where
  winDir w px [] = w
  winDir w px qs@((x1,y1):qs1) =
    if x0==x1 && y0>y1
    then let (nx,qs') = nextXL qs
         in winDir (w + (nx-px) `div` 2) x1 qs'
    else winDir w x1 qs1

  firstX = head $ filter (/=x0) $ map fst l
  lastX = head $ filter (/=x0) $ reverse $ map fst l

  nextXL [] = (firstX, [])
  nextXL (qs@((x1,y1):qs')) = if x1 == x0 then nextXL qs' else (x1,qs')

-- area contained inside a loop
enclosed :: [Point] -> [Point] -> [Point]
enclosed ground l =
  let cs = connected (ground \\ l)
      inside p = winding p l /= 0       
  in foldl (\en c -> if inside (head c) then c++en else en) [] cs

 -- connected components
connected :: [Point] -> [[Point]]
connected [] = []
connected (p:ps) = let (c0,ps0) = component p ps
                   in c0 : connected ps0 where
  component p = foldl (\(c0,ps0) p0 -> if p0 `near` c0 then (p0:c0,ps0) else (c0,p0:ps0))
                      ([p],[])

near :: Point -> [Point] -> Bool
near (x,y) ps = intersect [(x-1,y),(x+1,y),(x,y-1),(x,y+1)] ps /= [] 




-- AREA OF POLYGON

-- The shoelace formula for the area of a region
-- contained inside a polygon

-- double the area (to avoid rounding errors)
shoelace :: [Point] -> Int
shoelace ps = abs $ sum [(x0-x1)*(y0+y1) | ((x0,y0),(x1,y1)) <- zip ps (tail ps)]

-- integer points on a line (excluding last)
linePoints :: Point -> Point -> Int
linePoints (x0,y0) (x1,y1) = gcd (abs (x1-x0)) (abs (y1-y0))
  
-- integer points on a polygon (last point must be the same as first)
polyPoints :: [Point] -> Int
polyPoints ps = sum [linePoints p0 p1 | (p0,p1) <- zip ps (tail ps)]

-- by Pick's theorem A = i + b/2 - 1
--  where i = interior points, b = boundary points
-- so i = A - b/2 + 1, all points : i + b = A + b/2 + 1

polyInterior :: [Point] -> Int
polyInterior ps = (shoelace ps - polyPoints ps) `div` 2 + 1

polyArea :: [Point] -> Int
polyArea ps = (shoelace ps + polyPoints ps) `div` 2 + 1

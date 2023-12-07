-- Advent of Code: Useful Functions
--  Venanzio Capretta 2020 - 2023

module AoCTools where

import qualified Data.Map as M

-- ALREADY EXISTS: partition
spanBy :: (a->Bool) -> [a] -> ([a],[a])
spanBy p [] = ([],[])
spanBy p (x:xs) = let (ys,zs) = spanBy p xs
                  in if p x then (x:ys,zs)
                            else (ys,x:zs)

-- LISTS

-- apply a binary function element-wise
zipWithLong :: (a -> a -> a) -> [a] -> [a] -> [a] 
zipWithLong f [] ys = ys
zipWithLong f xs [] = xs
zipWithLong f (x:xs) (y:ys) = f x y : zipWithLong f xs ys

-- compose (forward) a list of functions
fsCompose :: [a -> a] -> a -> a 
fsCompose [] x = x
fsCompose (f:fs) x = fsCompose fs (f x)

-- already exists: mapMaybe in Data.Maybe

findMaybe :: (a -> Maybe b) -> [a] -> [b]
findMaybe f [] = []
findMaybe f (x:xs) = let fxs = findMaybe f xs in
  case (f x) of
    Just y -> y : fxs
    Nothing -> fxs

-- Minimum with a highest bound (for empty list)
minimumBound :: Ord a => a -> [a] -> a
minimumBound x = minimum . (x:)

-- Replace an element of a list (at given index)
replace :: Int -> a -> [a] -> [a]
replace i x l =
  let (front,back) = splitAt i l
  in front ++ x:(tail back)


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


-- Indexed maps

-- list to index map with indices as keys, starting at index i0
listMap :: Int -> [a] -> M.Map Int a
listMap i0 = M.fromAscList . (zip [i0..])

lMap :: [a] -> M.Map Int a
lMap = listMap 0

-- 2-dimentional matrix to index map, with coordinates as keys
matrixMap :: (Int,Int) -> [[a]] -> M.Map (Int,Int) a
matrixMap (i0,j0) xss = M.fromList [((i0+i,j0+j), xss!!j!!i) |
                                    j <- [0 .. length xss - 1],
                                    i <- [0 .. length (xss!!j) - 1]]
                        
mMap :: [[a]] -> M.Map (Int,Int) a
mMap = matrixMap (0,0)



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

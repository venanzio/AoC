-- Advent of Code: Useful Functions
--  Venanzio Capretta 2020 - 2025

module AoCTools_old where

import Data.List
import Data.Function (on)
import qualified Data.Map as M

-- NUMBERS

-- number of decimal digits of a natural
dig10 :: Int -> Int
dig10 n = if n<10 then 1 else 1 + dig10 (n `div` 10)

divisible :: Int -> Int -> Bool
divisible x y = rem x y == 0

-- LINEAR DIOPHANTINE EQUATIONS

-- Extended Euclidean Algorithm
-- returns the gcd g and coefficients s, t s.t. s*x+t*y = g

euclid :: Int -> Int -> (Int,Int,Int)
euclid x 0 = (x,1,0)
euclid x y = let (q,r) = quotRem x y
                 (g,s,t) = euclid y r
             in (g,t,s-t*q)
                
-- Linear Diophantine Equation in two variables
--   solve a*x + b*y = c
--   if solvable, returns a function giving all solutions

dioph2 :: Int -> Int -> Int -> Maybe (Int -> (Int,Int))
dioph2 a b c = let (d,ca,cb) = euclid a b
                   cm = c `div` d
                   x0 = ca*cm
                   y0 = cb*cm
                   cx = b `div` d
                   cy = a `div` d
  in if divisible c d then Just (\k -> (x0 + cx*k, y0 - cy*k)) else Nothing

  
{-************
  * GEOMETRY *
  ************-}

-- INDEXED MAPS

type Point = (Int,Int)
type Direction = Point

pX :: Point -> Int
pX = fst

pY :: Point -> Int
pY = snd

-- Manhattan distance
distM :: Point -> Point -> Int
distM (x1,y1) (x2,y2) = abs (x2-x1) + abs (y2-y1)

type Map2D a = M.Map Point a

-- user-friendly picture of a map

minMaxPoints :: [Point] -> (Point,Point)
minMaxPoints ps = ((minX,minY),(maxX,maxY))
  where xs = map pX ps
        ys = map pY ps
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys

  
showPoints :: Char -> [Point] -> String
showPoints c ps = unlines  [[sh (i,j) | i <- [minX..maxX]] | j <- [minY..maxY]]
  where ((minX,minY),(maxX,maxY)) = minMaxPoints ps
        sh p = if p `elem` ps then c else '.'

showManyPoints :: [(Char,[Point])] -> String
showManyPoints pmap =
  let ps = concat (map snd pmap)
      ((minX,minY),(maxX,maxY)) = minMaxPoints ps
      pchar p = head ([c | (c,qs) <- pmap, p `elem` qs] ++ ".")
  in unlines [[pchar (i,j) | i <- [minX..maxX]] | j <- [minY..maxY]]

showMap :: (a -> Char) -> Map2D a -> String
showMap sa m = unlines [[sha (i,j) | i <- [minX..maxX]] | j <- [minY..maxY]]
  where ((minX,minY),(maxX,maxY)) = minMaxPoints (M.keys m)
        sha p = case M.lookup p m of
          Nothing -> '.'
          Just x -> sa x

-- list to index map with indices as keys, starting at index i0
listMap :: Int -> [a] -> M.Map Int a
listMap i0 = M.fromAscList . (zip [i0..])

lMap :: [a] -> M.Map Int a
lMap = listMap 0

-- 2-dimentional matrix to index map, with coordinates as keys
matrixMap :: Point -> [[a]] -> Map2D a
matrixMap (i0,j0) xss = M.fromList [((i0+i,j0+j), xss!!j!!i) |
                                    j <- [0 .. length xss - 1],
                                    i <- [0 .. length (xss!!j) - 1]]
                        
mMap :: [[a]] -> Map2D a
mMap = matrixMap (0,0)

-- map by applying a Maybe-function to elements of a matrix
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

-- travelling in a 2-dimensional map
pMove :: Point -> Direction -> Point
pMove (x,y) (dx,dy) = (x+dx,y+dy)

extract :: Ord k => k -> M.Map k a -> Maybe (a,M.Map k a)
extract k m = case M.lookup k m of
  Nothing -> Nothing
  Just x -> return (x, M.delete k m)

-- move a point on a map
mMove :: Ord k => k -> k -> M.Map k a -> M.Map k a
mMove k1 k2 m = case extract k1 m of
  Nothing -> m
  Just (x,m0) -> M.insert k2 x m0

-- vector distance between two points
pDist :: Point -> Point -> Direction
pDist (x1,y1) (x2,y2) = (x2-x1,y2-y1)

-- opposite of a vector
pNeg :: Direction -> Direction
pNeg (v1,v2) = (-v1,-v2)

-- directions: up, down, left, right and diagonal
directions :: [Direction]
directions = [(dx,dy) | dx <- [-1..1], dy <- [-1..1]] \\ [(0,0)]

neighbours :: Point -> [Point]
neighbours p = map (pMove p) directions

dUp    = (0,-1) :: Direction
dDown  = (0,1)  :: Direction
dLeft  = (-1,0) :: Direction
dRight = (1,0)  :: Direction

directionsHV = [dUp,dDown,dLeft,dRight]

neighboursHV :: Point -> [Point]
neighboursHV p = map (pMove p) directionsHV

-- turning a right angle clockwise
dRTurn :: Direction -> Direction
dRTurn (x,y) = (-y,x)

-- turning anti-clockwise
dLTurn ::  Direction -> Direction
dLTurn (x,y) = (y,-x)

-- is a point inside a box with given up-left and downright corners?
pInside :: Point -> Point -> Point -> Bool
pInside (minX,minY) (maxX,maxY) (x,y) =
  minX <= x && x <= maxX && minY <= y && y <= maxY

-- moving in a direction and returning the list of elememts visited
-- WARNING: it doesn't work with sparse maps
--   delete the Nothing part and make it lazy-non-terminating?
mTrace :: Map2D a -> Point -> Point -> [a]
mTrace m p d = case M.lookup p m of
  Nothing -> []
  Just x -> x : mTrace m (pMove p d) d

-- count all occurrences of from any point to any direction
mOccurrences :: Eq a => [a] -> Map2D a -> Int
mOccurrences l m = length [(p,d) | p <- M.keys m, d <- directions,
                                   l `isPrefixOf` mTrace m p d]
-- occurrence of a submap at a point
subOccur :: Eq a => Map2D a -> Map2D a -> Point -> Bool
subOccur sub map p =
  let p0 = fst $ M.findMin sub
  in M.isSubmapOf (M.mapKeys (pMove (pDist p0 p)) sub) map

-- count all occurrences of a sub-map
subOccurrences :: Eq a => Map2D a -> Map2D a -> Int
subOccurrences sub map = length [p | p <- M.keys map, subOccur sub map p]

-- horizontal and vertical mirror images
hMirror :: Map2D a -> Map2D a
hMirror = M.mapKeys (\(i,j) -> (-i,j))

vMirror :: Map2D a -> Map2D a
vMirror = M.mapKeys (\(i,j) -> (i,-j))

-- swap up-down and left-righy
mTranspose :: Map2D a -> Map2D a
mTranspose = M.mapKeys (\(i,j) -> (j,i))

          
-- list of strings to 2D map ('.' means empty)
stringsMap :: [String] -> Map2D Char
stringsMap = M.filter (/='.') . mMap

-- list of positions of items satisfying a property
mSatisfy :: (a->Bool) -> Map2D a -> [Point]
mSatisfy prop = M.foldrWithKey (\p x ps -> if prop x then p:ps else ps) []

-- Find the position(s) of an item
mFind :: Eq a => a -> Map2D a -> [Point]
mFind x = mSatisfy (==x)


-- Winding number , copied from day 10 of 2023

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






{- *********
   * OTHER *
   *********
   To be reorganized -}

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

-- list is sorted according to a given ordering
isOrderedBy :: (a->a->Ordering) -> [a] -> Bool
isOrderedBy ord (x0:xs@(x1:_)) = ord x0 x1 == LT && isOrderedBy ord xs
isOrderedBy _ _ = True

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

--bounded maximum
maximumB :: Ord a => a -> [a] -> a
maximumB x xs = maximum (x:xs)


-- iterate a function until it satisfies a condition
iterSat :: (a -> Bool) -> (a -> a) -> a -> (Int,a)
iterSat cond f x = iterCount x 0 where
  iterCount x n = if cond x then (n,x) else iterCount (f x) (n+1)
  

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





-- WHEELS
-- Double linked circular lists
-- Implemented so all operations have amortized complexity O(1)

type Wheel a = ([a], [a])

-- A pair ([y1,..,yn],[z1,..,zm]) represents a circular list
-- with elements [y1,..,yn,zm,..,z1];
-- the element to the right of z1 is y1, the element to the left of y1 is z1.
-- The head element is y1


readW :: Wheel a -> a
readW (x:_,_) = x
readW ([],ys@(_:_)) = last ys 
readW _       = error "readW: empty wheel"

emptyW :: Wheel a
emptyW = ([],[])

-- A wheel with no head: first component empty
--  (A correct wheel should be empty if it has no head)
noHeadW :: Wheel a -> Bool
noHeadW w = null (fst w)

isEmptyW :: Wheel a -> Bool
isEmptyW ([],[]) = True
isEmptyW _ = False

-- Turning a list into a wheel
--   The last element of the list is linked back to the first
--   This "bends the list around clockwise"
listLW :: [a] -> Wheel a
listLW [x] = ([x],[])
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
rightW ([y],zs) = listRW (y:zs)
rightW (y:ys,zs) = (ys,y:zs)
rightW ([],[]) = ([],[])
rightW ([],zs) = rightW (listRW zs)

-- Move to the next element anti-clockwise
leftW :: Wheel a -> Wheel a
leftW (ys,z:zs) = (z:ys,zs)
leftW ([],[]) = ([],[])
leftW ([y],[]) = ([y],[])
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

-- readn the element on the Left of the Head
readLastW :: Wheel a -> a
readLastW (_,(x:_)) = x
readLastW (xs@(_:_),[]) = last xs
readLastW _ = error "readLastW: empty wheel"







-- TUPLES

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y

trd3 ::  (a,b,c) -> c
trd3 (_,_,z) = z

divide :: Eq a => a -> [a] -> [[a]]
divide _ [] = []
divide x l = case break (==x) l of
  (xs,[]) -> [xs]
  (xs,(_:ys)) -> xs : divide x ys

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (x,y) = (f x,y)


                            
-- DIJKSTRA ALGORITHM

infinite = maxBound `div` 2 :: Int

-- A graph maps a node to the nodes reachable in one step, with distance
type Graph a = M.Map a [(a,Int)]

type Queue a = M.Map a Int

relax :: Ord a => Graph a -> Queue a -> a -> Int -> Queue a
relax graph queue x dx =
  foldl (\q (y,dxy) -> M.adjust (\dy -> min dy (dx+dxy)) y q) queue (graph  M.! x)

-- Length of shortest path between two nodes
dijkstra :: Ord a => Graph a -> a -> a -> Int
dijkstra graph s t =
  dijkstra_aux $ M.fromList ((s,0) : [(v,infinite) | v <- M.keys graph, v /= s])
  where dijkstra_aux queue =
          let (x,dx) = minimumBy (compare `on` snd) (M.toAscList queue)
          in if x == t then dx
               else dijkstra_aux (relax graph (M.delete x queue) x dx)

-- to all targets
dijkstraAll :: Ord a => Graph a -> a -> M.Map a Int
dijkstraAll graph s =
  dijkstraAux $ M.fromList ((s,0) : [(v,infinite) | v <- M.keys graph, v /= s])
  where dijkstraAux queue = if null queue then M.empty else
          let (x,dx) = minimumBy (compare `on` snd) (M.toAscList queue)
          in M.insert x dx $ dijkstraAux (relax graph (M.delete x queue) x dx)



  
-- Version of Dijkstra also returning all the shortest paths

-- a queue maps a node to: distance from source, paths from source (reversed)
type QueueP a = M.Map a (Int,[[a]])

minD :: (a,(Int,[[a]])) -> (a,(Int,[[a]])) -> (a,(Int,[[a]]))
minD n1@(_,(d1,_)) n2@(_,(d2,_)) =
  if d1 <= d2 then n1 else n2

-- update elements of the queue with new node x
relaxP :: Ord a => Graph a -> QueueP a -> a -> (Int,[[a]]) -> QueueP a
relaxP graph queue x (dx,psx) =
  foldl (\q (y,dxy) -> M.adjust (\(dy,psy) -> bestPaths y dy psy dxy) y q)
        queue (graph M.! x)
  where bestPaths y dy psy dxy
          | dy' < dy = (dy', psy')
          | dy' > dy = (dy , psy )
          | otherwise = (dy, psy ++ psy')
          where dy' = dxy + dx
                psy' = map (y:) psx

-- Minumum distance in a non-empty queue
qMinimum :: QueueP a -> (a,(Int,[[a]]))
qMinimum queue = M.foldrWithKey (\x dpsx -> minD (x,dpsx))
                                (M.findMin queue) queue
                 
dijkstraPaths :: Ord a => Graph a -> a -> a -> (Int,[[a]])
dijkstraPaths graph s t = dijkstraAux queue0
  where queue0 = M.fromList ((s,(0,[[s]])) :
                             [(v,(infinite,[])) | v <- M.keys graph, v /= s])
        dijkstraAux queue =
          let (x,dpsx) = qMinimum queue
          in if x == t then dpsx
               else dijkstraAux (relaxP graph (M.delete x queue) x dpsx)

{-
dijkstra :: Ord a => Graph a -> a -> a -> Int
dijkstra graph s t = fst (dijkstraPaths graph s t)
-}

{- find any path -}
findPath :: Ord a => Graph a -> a -> a -> Maybe [a]
findPath graph s t = fpAux s (delete s $ M.keys graph)
  where fpAux s [t] = Just [t]
        fpAux s queue
          | t `elem` ns = Just [s,t]
          | ps == []    = Nothing
          | otherwise   = Just $ s : head ps
          where (ns,queue') = partition (`elem` map fst (graph M.! s)) queue
                ps = [p | Just p <- map (\n -> fpAux n queue') ns] 





-- Creating a graph from a maze

allPoints :: Point -> Point -> [Point]
allPoints (minX,minY) (maxX,maxY) = [(i,j) | i <- [minX..maxX], j <- [minY..maxY]]
  
mazeGraph :: [Point] -> Graph Point
mazeGraph wall = M.fromList [(p,next p) | p <- free]
  where (pMin,pMax) = minMaxPoints wall
        free = filter (not . flip elem wall) (allPoints pMin pMax)
        next p = [(q,1) | q <- neighboursHV p \\ wall]
 

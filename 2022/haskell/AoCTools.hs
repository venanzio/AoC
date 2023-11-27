-- Advent of Code: Useful Functions

module AoCTools where

import qualified Data.Map as M

-- ALREADY EXISTS: partition
spanBy :: (a->Bool) -> [a] -> ([a],[a])
spanBy p [] = ([],[])
spanBy p (x:xs) = let (ys,zs) = spanBy p xs
                  in if p x then (x:ys,zs)
                            else (ys,x:zs)

-- LISTS

-- Minimum with a highest bound (for empty list)
minimumBound :: Ord a => a -> [a] -> a
minimumBound x = minimum . (x:)

-- Replace an element of a list (at given index]
replace :: Int -> a -> [a] -> [a]
replace i x l =
  let (front,back) = splitAt i l
  in front ++ x:(tail back)

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

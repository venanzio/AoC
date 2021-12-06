-- Advent of Code: Useful Functions

module AoCTools where

import Data.Map as M

-- Iterating a function n times
nIter :: (a->a) -> Int -> a->a
nIter f 0 x = x
nIter f n x = nIter f (n-1) (f x)


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


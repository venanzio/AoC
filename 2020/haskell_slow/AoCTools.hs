-- Advent of Code: Useful Functions

module AoCTools where

import qualified Data.Map as M

-- Iterating a function n times
nIter :: (a->a) -> Int -> a->a
nIter f 0 x = x
nIter f n x = nIter f (n-1) (f x)

-- Keeping the Just values from a list of Maybe
filterJust :: [Maybe a] -> [a]
filterJust l = [x | Just x <- l]



-- Minimize a function over a list and return: index, element, function value
minimumF :: Ord b => (a->b) -> [a] -> (Int,a,b)
minimumF f (x:xs) =
  foldl (\(i,y,v) (j,z) -> let w = f z in
                           if w<v then (j,z,w) else (i,y,v))
        (0,x,f x) (zip [1..] xs)

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

--NUMBER THEORY

-- Are two numbers relatively prime?
relPrime :: Integral int => int -> int -> Bool
relPrime x y = gcd x y == 1

-- Are numbers in a list pairways relatively prime?
coPrimes :: Integral int => [int] -> Bool
coPrimes (x:xs) = all (relPrime x) xs && coPrimes xs
coPrimes [] = True

-- Extended Euclidean algorithm
--  returns (m,n,g) where g=gcd(x,y) and m*x + n*y = g
euclid :: Integral int => int -> int -> (int,int,int)
euclid x 0 = (1,0,x)
euclid x y = let d = x `div` y
                 r = x `mod` y
                 (a,b,g) = euclid y r
             in (b,a-b*d,g)

-- Chinese Reminder for two moduli:
--  if n1 n2 are relatively prime, find x (mod n1*n2) s.t.
--      x = a1 (mod n1), x = a2 (mod n2)
chinesePair :: Integral int => (int,int) -> (int,int) -> int
chinesePair (n1,a1) (n2,a2) =
  let (m1,m2,g) = euclid n1 n2
  in if g/=1 then error ("moduli " ++ show (toInteger n1) ++ " and " ++ show (toInteger n2) ++ " not relatively prime")
             else (a1*m2*n2 + a2*m1*n1) `mod` (n1*n2)

-- Chinese reminder coefficient for a list of modulus/reminder
chineseReminder :: Integral int => [(int,int)] -> int
chineseReminder [(n,a)] = a
chineseReminder ((n1,a1):(n2,a2):ps) = chineseReminder ((n1*n2,chinesePair (n1,a1) (n2,a2)):ps) 
   

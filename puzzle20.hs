module PuzzleInput where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M


puzzle :: String -> IO Int
puzzle fileName = do
  input <- readFile fileName
  let [(array,_)] = parse (some tileP) input
      ma = matchAll array
      ms = M.keys ma
      ids = map tid array
  return (product (corners array))


type Tile = (Int,[String])

tid :: Tile -> Int
tid (n,_) = n

side :: Tile -> Int -> String
side (_,sds) i = sds!!i

sides :: [String] -> [String]
sides sds = [head sds,map last sds,reverse $ last sds, reverse $ map head sds]


tileP :: Parser Tile
tileP = do
  symbol "Tile"
  id <- natural
  symbol ":"
  borders <- repN 10 line
  return (id,sides borders)

getTile :: [Tile] -> Int -> Tile
getTile ts i = head $ filter ((==i).tid) ts

-- Part 1

type TComp = (Int,Bool)  -- side and whether flipped or not

-- Comparison of sides: False is flipped
compSide :: String -> String -> Maybe Bool
compSide s1 s2
  | s1 == s2         = Just True
  | s1 == reverse s2 = Just False
  | otherwise        = Nothing

-- Comparing tiles: matching sides and flipped/not-flipped
compTiles :: Tile -> Tile -> Maybe (Int,Int,Bool)
compTiles t1 t2 = 
  case [(i,j,b) | (i,j) <- sideMatch,
                  Just b <- [compSide (side t1 i) (side t2 j)]
                  ] of
    [] -> Nothing
    (m:_) -> Just m

sideMatch :: [(Int,Int)]
sideMatch = [(i,j) | i<-[0..3], j<-[0..3]]


type Matching = M.Map (Int,Int) (Int,Int,Bool)

addMatch :: Tile -> Tile -> Matching -> Matching
addMatch t1 t2 = case compTiles t1 t2 of
  Nothing -> id
  Just (i,j,b) -> M.insert (tid t1,tid t2) (i,j,b) .
                  M.insert (tid t2,tid t1) (j,i,b) 

matchAll :: [Tile] -> Matching
matchAll = foldl (\m (t1,t2) -> addMatch t1 t2 m) M.empty . allPairs

allPairs :: [a] -> [(a,a)]
allPairs [] = []
allPairs (x:xs) = [(x,y) | y <- xs] ++ allPairs xs 

countM :: Int -> [(Int,Int)] -> Int
countM n = foldl (\c (i,j) -> if n==i then c+1 else c) 0

corners :: [Tile] -> [Int]
corners ts = let ms = M.keys $ matchAll ts
                 ids = map tid ts
             in filter (\i -> countM i ms == 2) ids

-- Part 2

-- Rotations and Reflections
type Transf = (Int,Bool)

-- (n,b) indicate n clockwise rotations followed
-- by a reflection on the main diagonal if b is true

-- rotate and transform
rotate :: Transf -> Transf
rotate (n,b) = ((n+1) `mod` 4, b)

-- mirror and transform (mirror+rotate = rotate back and mirror)
mirror :: Transf -> Transf
mirror (n,b) = ((4-n) `mod` 4,not b)

-- composing transformations
compT :: Transf -> Transf -> Transf
compT (0,True)  tr = mirror tr
compT (0,False) tr = tr
compT (n,b)     tr = rotate (compT (n-1,b) tr)     

-- Effect of transformations on sides

rotSide :: Int -> Int
rotSide k = (k+1) `mod` 4

mirSide :: Int -> Int
mirSide k = (4-k-1) `mod` 4

transf :: Transf -> Int -> Int
transf (0,False) k = k
transf (0,True)  k = mirSide k
transf (n,b)     k = transf (n-1,b) (rotSide k)


-- inverse transformation

invT :: Transf -> Transf
invT (n,False) = ((4-n) `mod` 4,False)
invT (n,True)  = compT (0,True) (invT (n,False))

chSide :: Transf -> Int -> Int  -- compute original side of rotated/mirrored tile
chSide tr k = transf (invT tr) k

{- given two tiles such that they match on sides i and j respectively
   with b indicating whether they are inverted,
   compute the rotations/reflection of the second that brings it
   to match properly
-}
matchS :: Int -> Int -> Bool -> (Int,Bool)
matchS 0 0 True  = (1,True)
matchS 0 0 False = (2,False)
matchS 0 1 True  = (0,True)
matchS 0 1 False = (1,False)
matchS 0 2 True  = (0,False)
matchS 0 2 False = (3,True)
matchS 0 3 True  = (3,False)
matchS 0 3 False = (2,True)
matchS 1 m b     = matchS 0 m b `compT` (1,False)
matchS 2 m b     = matchS 0 m b `compT` (2,False)
matchS 3 m b     = matchS 0 m b `compT` (3,False)


  
type RFTile = (Tile,Transf)  -- rotations and reflection
                             -- reflection last (True means reflection)


-- Transforming a tile
trPic :: Transf -> [String] -> [String]
trPic (0,True)  sds = transpose sds
trPic (0,False) sds = sds
trPic (n,b)     sds = trPic (n-1,b) $ transpose $ reverse sds

trTile :: Transf -> Tile -> Tile
trTile tr (n,sds) = (n,trPic tr sds)

-- Match the right side of a tile

matchR :: Tile -> Tile -> Maybe Transf
matchR t1 t2 = case compTiles t1 t2 of
    Just (1,n,b) -> Just matchL n b
    _            -> Nothing
  where matchL 0 True  = (0,True)
        matchL 0 False = (3,False)
        matchL 1 True  = (3,True)
        matchL 1 False = (2,False)
        matchL 2 True  = (1,False)
        matchL 2 False = (2,True)
        matchL 3 True  = (0,False)
        matchL 3 False = (1,True)

-- Match the bottom side of a tile

matchB :: Tile -> Tile -> Maybe Transf
matchB t1 t2 = case compTiles t1 t2 of
    Just (2,n,b) -> Just matchT n b
    _            -> Nothing
  where matchT 0 True  = (0,False)
        matchT 0 False = (3,True)
        matchT 1 True  = (3,False)
        matchT 1 False = (2,True)
        matchT 2 True  = (1,True)
        matchT 2 False = (2,False)
        matchT 3 True  = (0,True)
        matchT 3 False = (1,False)

tileR :: Tile -> [Tile] -> Maybe Tile
tileR t0 [] = Nothing
tileR t0 (t1:ts) = case

  

  


type Grid = M.Map (Int,Int) RFTile


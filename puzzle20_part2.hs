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
  putStrLn (show $ head array)
  return 0

type Square = [String]
type Tile = (Int,Square)

tileP :: Parser Tile
tileP = do
  symbol "Tile"
  id <- natural
  symbol ":"
  square <- repN 10 line
  return (id,square)

tid :: Tile -> Int
tid (n,_) = n

tsq :: Tile -> Square
tsq (n,sq) = sq

showSquare :: Square -> String
showSquare = intercalate "\n"

-- sides run clockwise starting from the top-left corner
side :: Square -> Int -> String
side sq 0 = head sq
side sq 1 = map last sq
side sq 2 = reverse (last sq)
side sq 3 = reverse (map head sq)

-- mirror image arount the top-left to bottom-right diagonal
mirror :: Square -> Square
mirror = transpose

-- clockwise rotation of a square
-- it is the same as horizontal flipping + diagonal mirror
rotate :: Square -> Square
rotate = mirror . reverse

-- All possible 8 transformations of a square
allTrans :: Square -> [Square]
allTrans sq = rots ++ map mirror rots
  where rots = take 4 (iterate rotate sq)

-- match and transform a square so its n-side matches the given one
matchSq :: String -> Int -> Square -> Maybe Square
matchSq sd i = listMaybe . filter (\sq -> side sq i == sd) . allTrans

listMaybe :: [a] -> Maybe a
listMaybe [] = Nothing
listMaybe (x:_) = Just x

-- placing square sq1 on side i of square sq0
placeSq :: Square -> Int -> Square -> Maybe Square
placeSq sq0 i sq1 = matchSq (reverse $ side sq0 i) (i+2 `mod` 4) sq1

-- Constructing a grid from a list of tiles

type Position = (Int,Int)
type Grid = M.Map Position Tile

placeTile :: (Position,Tile) -> Int -> Tile -> Maybe (Position,Tile)
placeTile (pos,t0) i t1 =
  let id0 = tid t0
      sq0 = tsq t0
      id1 = tid t1
      sq1 = tsq t1
  in case placeSq sq0 i sq1 of
    Nothing -> Nothing
    Just sq2 -> Just (posSide pos i,(id1,sq2))



-- next position on a given side
posSide :: Position -> Int -> Position
posSide (i,j) 0 = (i-1,j  )  -- up
posSide (i,j) 1 = (i  ,j+1)  -- right
posSide (i,j) 2 = (i+1,j  )  -- down
posSide (i,j) 3 = (i  ,j-1)  -- left

extract :: (a -> Maybe b) -> [a] -> (Maybe b,[a])
extract f [] = (Nothing, [])
extract f (x:xs) = case f x of
  Nothing -> let (mx,ys) = extract f xs in (mx,x:ys)
  mx      -> (mx,xs)

insertMaybe :: Ord a => Maybe (a,b) -> M.Map a b -> M.Map a b
insertMaybe Nothing = id
insertMaybe (Just (a,b)) = M.insert a b

compose :: [a->a] -> a -> a
compose [] = id
compose (f:fs) = f . compose fs

tileGrid :: (Position,Tile) -> ([Tile],Grid) -> ([Tile],Grid)
tileGrid pt = compose $ map (tileSGrid pt) [0..3]

tileSGrid :: (Position,Tile) -> Int -> ([Tile],Grid) -> ([Tile],Grid)
tileSGrid (pos,t) i (ts,gr) =
  let (mpt,ts') = extract (placeTile (pos,t) i) ts
  in case mpt of
    Nothing -> (ts',gr)
    Just (pos',t') -> tileGrid (pos',t') (ts',M.insert pos' t' gr)

grid :: [Tile] -> Grid
grid (t:ts) = snd (tileGrid ((0,0),t) (ts,M.empty))



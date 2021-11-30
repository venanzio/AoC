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
      gr = grid array
      assL = M.assocs gr
      squares = map (\(pos,tl) -> (pos,dimensions (tsq tl))) (M.assocs gr)
      pic = gridPicture gr
  putStrLn ("there are " ++ (show $ countAll monster pic) ++ " monsters")
  -- return (product $ map tid (corners gr))
  return (countHash pic - 15 * countAll monster pic)

type Square = [String]
type Tile = (Int,Square)

dimensions :: Square -> (Int,Int)
dimensions sq = (length sq, length (head sq))

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
side sq n = error ("Tried to take side " ++ show n ++
                   " of square \n" ++ showSquare sq)

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
matchSq sd i
  | i<4 = listMaybe . filter (\sq -> side sq i == sd) . allTrans
  | otherwise = error ("matchSq: side "++ show i)
  
listMaybe :: [a] -> Maybe a
listMaybe [] = Nothing
listMaybe (x:_) = Just x

-- placing square sq1 on side i of square sq0
placeSq :: Square -> Int -> Square -> Maybe Square
placeSq sq0 i sq1 = matchSq (reverse $ side sq0 i) ((i+2) `mod` 4) sq1

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
grid (t:ts) = snd (tileGrid ((0,0),t) (ts,M.insert (0,0) t M.empty))

-- Part 1

corners :: Grid -> [Tile]
corners gr = let grL = M.assocs gr
                 ((i0,j0),t0) = head grL
                 ((i1,j1),t3) = last grL
                 Just t1 = M.lookup (i0,j1) gr
                 Just t2 = M.lookup (i1,j0) gr
             in [t0,t1,t2,t3]

-- Part 2

-- Recombining the grid

glue :: [String] -> [String] -> [String]
glue (x:xs) (y:ys) = (x++y) : (glue xs ys)
glue xs [] = xs
glue _ ys = ys

glueSq :: [Square] -> [String]
glueSq = foldl glue []

paste :: [[Square]] -> [String]
paste = concat . map glueSq

-- Assuming the positions are ordered,    
byRow :: [(Position,a)] -> [[a]]
byRow [] = []
byRow xs = let (ys,zs) = span (\(p,_) -> fst p == fst (fst $ head xs)) xs
           in map snd ys : byRow zs

-- Removing the border of the picture

dropLast :: [a] -> [a]
dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x:dropLast xs

removeBorder :: Square -> Square
removeBorder = map tail . map dropLast . tail . dropLast

-- Recovering the picture from the grid

gridPicture :: Grid -> [String]
gridPicture gr =
  let tiles = M.assocs gr
      squares = map (\(pos,tile) -> (pos, removeBorder (tsq tile))) tiles
      arraySq = byRow squares
  in paste arraySq

-- Finding the monster

monster :: [String]
monster = ["                  # ", 
           "#    ##    ##    ###",
           " #  #  #  #  #  #   "]

-- Positions of hashes in a picture
picPos :: [String] -> [Position]
picPos pic = [(i,j) | i <- [0..length pic -1], j <- [0..length (pic!!i) -1],
                      pic!!i!!j == '#']

picAll :: [String] -> [[Position]]
picAll pic = map picPos pics3
  where pics1 = [pic,transpose pic]
        pics2 = map reverse pics1 ++ pics1
        pics3 = map (map reverse) pics2 ++ pics2

projM :: [a] -> Int -> Maybe a
projM xs j = if j < length xs then Just (xs!!j) else Nothing

elPos :: [[a]] -> Position -> Maybe a
elPos xss (i,j) = if i < length xss then projM (xss!!i) j else Nothing

posPlus :: Position -> Position -> Position
posPlus (i0,j0) (i1,j1) = (i0+i1,j0+j1)

-- check if there is a # at a certain position from a starting position
checkPos :: [String] -> Position -> Bool
checkPos pic pos = elPos pic pos == Just '#'


findPicPos :: [Position] -> Position -> [String] -> Bool
findPicPos ps pos pic = all (checkPos pic . posPlus pos) ps

countSat :: (a -> Bool) -> [a] -> Int
countSat p xs = length (filter p xs)

countPics :: [Position] -> [String] -> Int
countPics ps pic = countSat (\pos -> findPicPos ps pos pic)
                            [(i,j) | i <- [0..length pic-1],
                                     j <- [0..length (pic!!i) -1]]

countAll :: [String] -> [String] -> Int
countAll mstr pic = sum (map (\ps -> countPics ps pic) (picAll mstr))

countHash :: [String] -> Int
countHash = length . filter (=='#') . concat



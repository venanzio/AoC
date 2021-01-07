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
  let (d1,d2) = parseAll decks input
      (_,xs) = playR (d1,d2) -- play (d1,d2)
  return (score xs)

plDeck :: Int -> Parser [Int]
plDeck ps = do
  symbol "Player"
  n <- natural
  symbol ":"
  cards <- some natural
  if n==ps then return cards else empty

decks :: Parser ([Int],[Int])
decks = do d1 <- plDeck 1
           d2 <- plDeck 2
           return (d1,d2)

-- Part 1

plRound :: ([Int],[Int]) -> ([Int],[Int])
plRound ((x:xs),(y:ys)) = if x>y then (xs++[x,y],ys) else (xs,ys++[y,x])
plRound _ = error "Empty deck"

play :: ([Int],[Int]) -> (Bool,[Int])
play (xs, []) = (True,xs)
play ([], ys) = (False,ys)
play (xs, ys) = play $ plRound (xs,ys)

score :: [Int] -> Int
score xs = foldl (\s (x,k) -> s+x*k) 0 $ zip (reverse xs) [1..]

-- Part 2

type Decks = ([Int],[Int])
type History = [Decks]

emptyHistory :: History
emptyHistory = []

checkHistory :: Decks -> History -> Maybe History
checkHistory ds h = if ds `elem` h then Nothing else Just (ds:h)

data GameState = GS {decksGS :: Decks,
                     historyGS :: History}

roundWin :: Decks -> Bool
roundWin ((x:xs),(y:ys))
  | x <= length xs && y <= length ys = fst (playR (take x xs,take y ys))
  | otherwise = x>y

rRound :: Decks -> Decks
rRound ds@((x:xs),(y:ys)) =
  if roundWin ds then (xs++[x,y],ys) else (xs,ys++[y,x])
rRound _ = error "Empty deck"

rPlay :: GameState -> (Bool,[Int])
rPlay gs = case checkHistory (decksGS gs) (historyGS gs) of
  Nothing -> (True,fst (decksGS gs))
  Just h' -> if (fst (decksGS gs)) == []
             then (False, snd (decksGS gs))
             else if (snd (decksGS gs)) == []
                  then (True, fst (decksGS gs))
                  else let ds = rRound (decksGS gs)
                       in rPlay (GS {decksGS = ds,
                                     historyGS = (decksGS gs) : historyGS gs})
playR :: Decks -> (Bool,[Int])
playR ds = rPlay (GS {decksGS = ds, historyGS = emptyHistory})

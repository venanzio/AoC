-- Advent of Code 2024, day 15
--  Venanzio Capretta

module Main where

import System.Environment
import Control.Applicative
import qualified Data.Map as M

import FunParser
import AoCTools

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let (h,ms) = parseAll pInput input
  -- stepMoves (robot h) ms h
  putStrLn ("Part 1: " ++ show (part1 h ms))
  let wh = largeWH h
      wh' = movesRobot (robot wh) ms wh
  -- stepWH (robot wh) ms wh
  putStrLn ("Part 2: " ++ show (part2 wh ms))

-- Parsing the input

pMapLine :: Parser String
pMapLine = do char '#'
              s <- chars
              return ('#':s)

pHouse :: Parser (Map2D Char)
pHouse = do some pMapLine >>= return.stringsMap
             
pMoves :: Parser String
pMoves = some (choice (map (token.char) "^v<>"))

pInput :: Parser (Map2D Char, String)
pInput = do h <- pHouse
            ms <- pMoves
            return (h,ms)

-- Part 1

robot :: Map2D Char -> Point
robot = head.(mFind '@')

mDir :: Char -> Direction
mDir '^' = dUp
mDir 'v' = dDown
mDir '<' = dLeft
mDir '>' = dRight

moveH :: Point -> Direction -> Map2D Char -> Maybe (Map2D Char)
moveH p d h = let p0 = pMove p d in
  case M.lookup p0 h of
    Nothing -> return $ mMove p p0 h
    Just '#' -> Nothing
    Just 'O' -> do h0 <- moveH p0 d h
                   return $ mMove p p0 h0
    _ -> error ("unexpected character at "++show p0)

moveR :: Point -> Direction -> Map2D Char -> (Point,Map2D Char)
moveR p d h = case moveH p d h of
  Nothing -> (p,h)
  Just h0 -> (pMove p d,h0)

stepMoves :: Point -> String -> Map2D Char -> IO ()
stepMoves p [] h = putStrLn (showMap id h)
stepMoves p (m:ms) h  = do
    putStrLn (showMap id h)
    putStrLn (show p ++ m:"  press any key to step")
    getChar
    let d = mDir m
        (p0,h0) = moveR p d h
    stepMoves p0 ms h0

movesR :: Point -> String -> Map2D Char -> Map2D Char
movesR p [] h = h
movesR p (m:ms) h = let d = mDir m
                        (p0,h0) = moveR p d h
                    in movesR p0 ms h0

gps :: Point -> Int
gps (x,y) = 100*y + x

part1 :: Map2D Char -> String -> Int
part1 h ms = sum $ map gps $ mFind 'O' (movesR (robot h) ms h)

-- Part 2

largeWH :: Map2D Char -> Map2D Char
largeWH = M.foldrWithKey enlarge M.empty where
  enlarge (x,y) '#' = M.insert (2*x,y) '#' . M.insert (2*x+1,y) '#'
  enlarge (x,y) 'O' = M.insert (2*x,y) '[' . M.insert (2*x+1,y) ']'
  enlarge (x,y) '@' = M.insert (2*x,y) '@'
e (x,y) '@' = M.insert (2*x,y) '@'

-- moving a box with its left edge at p
moveBox :: Point -> Direction -> Map2D Char -> Maybe (Map2D Char)
moveBox p d wh
  | d == dLeft = do
      wh0 <- case M.lookup p0 wh of
               Nothing  -> return wh 
               Just ']' -> moveBox (pMove p0 dLeft) dLeft wh
               _        -> empty
      return (mMove p' p0' $ mMove p p0 wh0)
  | d == dRight = do
      wh0 <- case M.lookup p0' wh of
               Nothing  -> return wh 
               Just '[' -> moveBox p0' dRight wh
               _        -> empty
      return (mMove p p0 $ mMove p' p0' wh0)
  | otherwise = do
      wh0 <- case M.lookup p0 wh of
               Nothing  -> return wh
               Just '[' -> moveBox p0 d wh
               Just ']' -> moveBox (pMove p0 dLeft) d wh
               _        -> empty
      wh1 <- case M.lookup p0' wh0 of
               Nothing  -> return wh0
               Just ']' -> return wh0
               Just '[' -> moveBox p0' d wh0
               _        -> empty
      return (mMove p p0 $ mMove p' p0' wh1)
  where p'  = pMove p dRight
        p0  = pMove p d
        p0' = pMove p' d

-- moving the robot, shifting boxes if needed        
moveWH :: Point -> Direction -> Map2D Char -> Maybe (Map2D Char)
moveWH p d wh = do
  let p0 = pMove p d
  case M.lookup p0 wh of
    Nothing -> return $ mMove p p0 wh
    Just '#' -> empty
    Just '[' -> do wh0 <- moveBox p0 d wh
                   return $ mMove p p0 wh0
    Just ']' -> do wh0 <- moveBox (pMove p0 dLeft) d wh
                   return $ mMove p p0 wh0
    _ -> error ("unexpected character at "++show p0)


moveRobot :: Point -> Direction -> Map2D Char -> (Point,Map2D Char)
moveRobot p d h = case moveWH p d h of
  Nothing -> (p,h)
  Just h0 -> (pMove p d,h0)

movesRobot :: Point -> String -> Map2D Char -> Map2D Char
movesRobot p [] wh = wh
movesRobot p (m:ms) wh = let d = mDir m
                             (p0,wh0) = moveRobot p d wh
                         in movesRobot p0 ms wh0

stepWH :: Point -> String -> Map2D Char -> IO ()
stepWH p [] h = putStrLn (showMap id h)
stepWH p (m:ms) h  = do
    putStrLn (showMap id h)
    putStrLn (show p ++ m:"  press any key to step")
    getChar
    let d = mDir m
        (p0,h0) = moveRobot p d h
    stepWH p0 ms h0
    
part2 :: Map2D Char -> String -> Int
part2 wh ms = sum $ map gps $ mFind '[' (movesRobot (robot wh) ms wh)

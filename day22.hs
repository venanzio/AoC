-- Advent of Code 2021, day 22

module Main where

import System.Environment
import Data.List
import Data.Char

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
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type Range = (Int,Int)
type Cuboid = (Range,Range,Range)
type Coordinates = (Int,Int,Int)

pCuboid :: Parser (Bool,Cuboid)
pCuboid = do switch <- (symbol "on" >> return True) <|> (symbol "off" >> return False)
             symbol "x="
             x0 <- integer
             symbol ".."
             x1 <- integer
             symbol ",y="
             y0 <- integer
             symbol ".."
             y1 <- integer
             symbol ",z="
             z0 <- integer
             symbol ".."
             z1 <- integer
             return (switch,((x0,x1),(y0,y1),(z0,z1)))

pInput :: Parser [(Bool,Cuboid)]
pInput = pLines pCuboid

-- Part 1

type Reactor = M.Map Coordinates Bool

initR :: Range -> Reactor
initR r = M.fromList $ map (\c -> (c,False)) (cubes (r,r,r))

shrinkRange :: Range -> Range -> Range
shrinkRange (u0,u1) (x0,x1) = (max u0 x0, min u1 x1)

shrinkCuboid :: Range -> Cuboid -> Cuboid
shrinkCuboid u (rx,ry,rz) = (shrinkRange u rx, shrinkRange u ry, shrinkRange u rz)

range :: Range -> [Int]
range (x0,x1) = [x0..x1]

cubes :: Cuboid -> [Coordinates]
cubes (rx,ry,rz) = [(x,y,z) | x <- range rx, y <- range ry, z <- range rz]


setCuboid :: Cuboid -> Bool -> Reactor -> Reactor
setCuboid cuboid b reactor =
  foldl (\r c -> M.insert c b r) reactor (cubes cuboid)


stepC :: Range -> (Bool,Cuboid) -> Reactor -> Reactor
stepC r (b,cuboid) = setCuboid (shrinkCuboid r cuboid) b

shrinkInput :: [(Bool,Cuboid)] -> [(Bool,Cuboid)]
shrinkInput [] = []
shrinkInput ((b,c):rest) =
  let c' = shrinkCuboid (-50,50) c
  in if isEmptyC c' then shrinkInput rest
                    else (b,c'):shrinkInput rest

part1 :: [(Bool,Cuboid)] -> Int
-- part1 = M.size . M.filter id . foldl (flip (stepC (-50,50))) (initR (-50,50))
part1 = part2 . shrinkInput



-- Part 2

inRange :: Int -> Range -> Bool
inRange x (x0,x1) = x0 <= x && x <= x1

inCuboid :: Coordinates -> Cuboid -> Bool
inCuboid (x,y,z) (rx,ry,rz) = inRange x rx && inRange y ry && inRange z rz

emptyR :: Range
emptyR = (1,-1)

rangeDiff :: Range -> Range -> [Range]
rangeDiff (x0,x1) (u0,u1)
  | x1 < x0 = [emptyR, emptyR, emptyR]
  | u1 < u0 || u1 < x0 || x1 < u0 = [(x0,x1), emptyR, emptyR]
  | u0 <= x0 = if u1<=x1 then [emptyR, (x0,u1), (u1+1,x1)]
                         else [emptyR, (x0,x1), emptyR]
  | otherwise = if u1<=x1 then [(x0,u0-1),(u0,u1),(u1+1,x1)]
                          else [(x0,u0-1),(u0,x1),emptyR]

isEmptyR :: Range -> Bool
isEmptyR (x0,x1) = x1 < x0

isEmptyC :: Cuboid -> Bool
isEmptyC (rx,ry,rz) = isEmptyR rx || isEmptyR ry || isEmptyR rz

cuboidDiff :: Cuboid -> Cuboid -> [Cuboid]
cuboidDiff (rx,ry,rz) (ru,rv,rw) = filter (not . isEmptyC) $
  let rdx = rangeDiff rx ru
      rdy = rangeDiff ry rv
      rdz = rangeDiff rz rw
  in [ (rdx!!0, ry, rz),
       (rdx!!1, rdy!!0, rz),
       (rdx!!1, rdy!!1, rdz!!0),
       (rdx!!1, rdy!!1, rdz!!2),
       (rdx!!1, rdy!!2, rz),
       (rdx!!2, ry, rz)
     ]

cuboidStep :: [Cuboid] -> (Bool,Cuboid) -> [Cuboid]
cuboidStep cs (b,cub) =
  let newcs = concat (map (\c -> cuboidDiff c cub) cs)
  in if b then cub:newcs else newcs

sizeR :: Range -> Int
sizeR (x0,x1) = if x0 <= x1 then x1-x0+1 else 0

sizeC :: Cuboid -> Int
sizeC (rx,ry,rz) = sizeR rx * sizeR ry * sizeR rz

sizeCs :: [Cuboid] -> Int
sizeCs = sum . map sizeC

part2 :: [(Bool,Cuboid)] -> Int
part2 cs = sizeCs (foldl cuboidStep [] cs)


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

part1 :: [(Bool,Cuboid)] -> Int
part1 = M.size . M.filter id . foldl (flip (stepC (-50,50))) (initR (-50,50))

-- Part 2

-- IDEA : instead of computing each cube, match cuboid two by two as you add them
-- keeping track of how many cubes you add or remove

part2 :: [(Bool,Cuboid)] -> Int
part2 = undefined

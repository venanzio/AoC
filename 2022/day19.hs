-- Advent of Code 2022, day 19
--  Venanzio Capretta

module Main where

import System.Environment
import Data.List
import Data.Char

import Control.Applicative
import qualified Data.Map as M

import FunParser
import AoCTools
import State

main :: IO ()
main = do
  args <- getArgs
  puzzle (head args)

puzzle :: String -> IO ()
puzzle fileName = do
  input <- readFile fileName
  let xs = parseAll pInput input
  putStrLn (show (map showBlueprint xs))
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

data RobotKind = OreR | ClayR | ObsidianR | GeodeR
  deriving (Show,Eq)

data Material = Material {
  mOre :: Int,
  mClay :: Int,
  mObsidian :: Int,
  mGeode :: Int }
  deriving (Show,Eq)

data Robot = Robot { -- kind of robot and costs to build it
  rKind :: RobotKind,
  rCost :: Material }
  deriving (Show,Eq)

type Blueprint = RobotKind -> Material

showBlueprint :: Blueprint -> String
showBlueprint bl = "ore robot: " ++ (show (bl OreR)) ++
                   "; clay robot: " ++ (show (bl ClayR)) ++
                   "; obsidian robot: " ++ (show (bl ObsidianR)) ++
                   "; geode robot: " ++ (show (bl GeodeR))

pCost :: Parser Material
pCost = do cl <- someSepStr (natural >>= \c -> word >>= \w -> return (w,c))
                            "and"
           let oreC = unJustD 0 (lookup "ore" cl)
               clayC = unJustD 0 (lookup "clay" cl)
               obsidianC = unJustD 0 (lookup "obsidian" cl)
           return (Material oreC clayC obsidianC 0)
  
pBlueprint :: Parser Blueprint
pBlueprint = do symbol "Blueprint"
                natural
                symbol ":"
                symbol "Each ore robot costs"
                oreC <- pCost
                symbol "."
                symbol "Each clay robot costs"
                clayC <- pCost
                symbol "."
                symbol "Each obsidian robot costs"
                obsidianC <- pCost
                symbol "."
                symbol "Each geode robot costs"
                geodeC <- pCost
                symbol "."
                return (\rk -> case rk of
                           OreR -> oreC
                           ClayR -> clayC
                           ObsidianR -> obsidianC
                           GeodeR -> geodeC)


pInput :: Parser [Blueprint]
pInput = some pBlueprint

-- Part 1

part1 :: [Blueprint] -> Int
part1 _ = 1

-- Part 2

part2 :: [Blueprint] -> Int
part2 _ = 2

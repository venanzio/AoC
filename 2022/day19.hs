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

noMaterial :: Material
noMaterial = Material {mOre = 0, mClay = 0, mObsidian = 0, mGeode = 0}

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

data RNumbers = RNumbers {
  rnOre :: Int,
  rnClay :: Int,
  rnObsidian :: Int,
  rnGeode :: Int}

justOneOreRobot :: RNumbers
justOneOreRobot =
  RNumbers {rnOre = 1, rnClay = 0, rnObsidian = 0, rnGeode = 0}

type BMState = (Blueprint,Material,RNumbers)

getMaterial :: Material -> Material -> Maybe Material
getMaterial needed available =
  if mOre available <= mOre needed &&
     mClay available <= mClay needed &&
     mObsidian available <= mObsidian needed
  then Just $ Material {mOre = mOre available - mOre needed,
                        mClay = mClay available - mClay needed,
                        mObsidian = mObsidian available - mObsidian needed,
                        mGeode = mGeode available}
  else Nothing

-- n is the number of robots requiring the needed material
getMn :: Int -> Material -> Material -> (Int,Material)
getMn 0 _ available = (0,available)
getMn n needed available = case getMaterial needed available of
  Nothing -> (0,available)
  Just available1 -> let (m,available2) =  getMn (n-1) needed available1
                     in  (m+1, available2)

-- Strategy: always use the higher robots first
bRun :: Int -> ST BMState Int
bRun 0 = do (_,m,rn) <- stState
            return (mGeode m)
bRun n = do (bp,m,rn) <- stState
            let (newGeodeRs,m1) = getMn (rnGeode rn) (bp GeodeR) m
                (newObsidianRs,m2) = getMn (rnObsidian rn) (bp ObsidianR) m1
                (newClayRs,m3) = getMn (rnClay rn) (bp ClayR) m2
                (newOreRs,m4) = getMn (rnOre rn) (bp OreR) m3
                m' = Material {mOre = mOre m4 + (rnOre rn),
                               mClay = mClay m4 + (rnClay rn),
                               mObsidian = mObsidian m4 + (rnObsidian rn),
                               mGeode = mGeode m4 + (rnGeode rn)}
                rn' = RNumbers {rnOre = rnOre rn + newOreRs,
                                rnClay = rnClay rn + newClayRs,
                                rnObsidian = rnObsidian rn + newObsidianRs,
                                rnGeode = rnGeode rn + newGeodeRs}
            stUpdate (bp,m',rn')
            bRun (n-1)
            
bGeodes :: Blueprint -> Int
bGeodes bp = stRun (bRun 24) (bp,noMaterial,justOneOreRobot)

part1 :: [Blueprint] -> Int
part1 bps = bGeodes (bps!!0)

-- Part 2

part2 :: [Blueprint] -> Int
part2 _ = 2

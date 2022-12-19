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
  deriving (Show,Eq)

justOneOreRobot :: RNumbers
justOneOreRobot =
  RNumbers {rnOre = 1, rnClay = 0, rnObsidian = 0, rnGeode = 0}

type BMState = (Blueprint,Material,RNumbers)

mUpdate :: Material -> ST BMState ()
mUpdate m = do (bp,_,rn) <- stState
               stUpdate (bp,m,rn)

rnUpdate :: RNumbers -> ST BMState ()
rnUpdate rn = do (bp,m,_) <- stState
                 stUpdate (bp,m,rn)


getMaterial :: Material -> Material -> Maybe Material
getMaterial needed available =
  if mOre needed <= mOre available &&
     mClay needed <= mClay available &&
     mObsidian needed <= mObsidian available
  then Just $ Material {mOre = mOre available - mOre needed,
                        mClay = mClay available - mClay needed,
                        mObsidian = mObsidian available - mObsidian needed,
                        mGeode = mGeode available}
  else Nothing

collectMaterial :: ST BMState ()
collectMaterial = do
  (bp,m,rn) <- stState
  let m' = Material {mOre = mOre m + (rnOre rn),
                     mClay = mClay m + (rnClay rn),
                     mObsidian = mObsidian m + (rnObsidian rn),
                     mGeode = mGeode m + (rnGeode rn)}
  mUpdate m'

makeOreR :: ST BMState Bool
makeOreR = do
  (bp,m,rn) <- stState
  let needed = bp OreR
  case getMaterial needed m of
    Nothing -> return False
    Just m' -> do
      let rn' = rn {rnOre = rnOre rn +1}
      mUpdate m'
      collectMaterial
      rnUpdate rn'
      return True

makeClayR :: ST BMState Bool
makeClayR = do
  (bp,m,rn) <- stState
  let needed = bp ClayR
  case getMaterial needed m of
    Nothing -> return False
    Just m' -> do
      let rn' = rn {rnClay = rnClay rn +1}
      mUpdate m'
      collectMaterial
      rnUpdate rn'
      return True

makeObsidianR :: ST BMState Bool
makeObsidianR = do
  (bp,m,rn) <- stState
  let needed = bp ObsidianR
  case getMaterial needed m of
    Nothing -> return False
    Just m' -> do
      let rn' = rn {rnObsidian = rnObsidian rn +1}
      mUpdate m'
      collectMaterial
      rnUpdate rn'
      return True

makeGeodeR :: ST BMState Bool
makeGeodeR = do
  (bp,m,rn) <- stState
  let needed = bp GeodeR
  case getMaterial needed m of
    Nothing -> return False
    Just m' -> do
      let rn' = rn {rnGeode = rnGeode rn +1}
      mUpdate m'
      collectMaterial
      rnUpdate rn'
      return True

stepGeode :: ST BMState Bool
stepGeode = do
  b <- makeGeodeR
  if b
    then return ()
    else do (bp,m,_) <- stState
            if mObsidian m < mObsidian (bp GeodeR)
              then stepObsidian
              else stepOre
  return b

stepObsidian :: ST BMState ()
stepObsidian = do
  b <- makeObsidianR
  if b then return ()
    else do (bp,m,_) <- stState
            if mClay m < mClay (bp ObsidianR)
              then stepClay
              else stepOre

stepClay :: ST BMState ()
stepClay = do
  b <- makeClayR
  if b then return ()
    else stepOre

stepOre :: ST BMState ()
stepOre = do
  b <- makeOreR
  if b then return ()
    else collectMaterial


bRun :: Int -> ST BMState Int
bRun 0 = do (_,m,_) <- stState
            return (mGeode m)
bRun n = do st <- stState
            let (b,stG) = app stepGeode st
                (_,stC) = app collectMaterial st
                nG = stRun (bRun (n-1)) stG
            return (if b
                      then nG
                      else max nG (stRun (bRun (n-1)) stC))
                 
bGeodes :: Blueprint -> Int
bGeodes bp = fst $ app (bRun 24) (bp,noMaterial,justOneOreRobot)
             
                       
part1 :: [Blueprint] -> Int
part1 bps = bGeodes (bps!!0)

-- Part 2

part2 :: [Blueprint] -> Int
part2 _ = 2

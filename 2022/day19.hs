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
      stUpdate (bp,m',rn')
      collectMaterial
      return True

makeClayR :: ST BMState Bool
makeClayR = do
  (bp,m,rn) <- stState
  let needed = bp ClayR
  case getMaterial needed m of
    Nothing -> return False
    Just m' -> do
      let rn' = rn {rnClay = rnClay rn +1}
      stUpdate (bp,m',rn')
      collectMaterial
      return True

makeObsidianR :: ST BMState Bool
makeObsidianR = do
  (bp,m,rn) <- stState
  let needed = bp ObsidianR
  case getMaterial needed m of
    Nothing -> return False
    Just m' -> do
      let rn' = rn {rnObsidian = rnObsidian rn +1}
      stUpdate (bp,m',rn')
      collectMaterial
      return True

makeGeodeR :: ST BMState Bool
makeGeodeR = do
  (bp,m,rn) <- stState
  let needed = bp GeodeR
  case getMaterial needed m of
    Nothing -> return False
    Just m' -> do
      let rn' = rn {rnGeode = rnGeode rn +1}
      stUpdate (bp,m',rn')
      collectMaterial
      return True


bRun :: Int -> BMState -> Int
bRun 0 (_,m,_) = mGeode m
bRun n st = let (bOr,stOr) = app makeOreR st
                (bCl,stCl) = app makeClayR st
                (bOb,stOb) = app makeObsidianR st
                (bG,stG)   = app makeGeodeR st
                ((),stNoR) = app collectMaterial st
                bsts = if bG
                         then [(bG,stG)]
                         else [(bOr,stOr),(bCl,stCl),
                               (bOb,stOb),(True,stNoR)]
                sts = [ m | (b,m) <- bsts , b]
            in maximum (map (\st -> bRun (n-1) st) sts)
                 

{-
-- n is the number of robots requiring the needed material
getMAll :: Material -> Material -> (Int,Material)
getMAll needed available = case getMaterial needed available of
  Nothing -> (0,available)
  Just available1 -> let (m,available2) =  getMAll needed available1
                     in  (m+1, available2)

-- Get the highest possible robot from the available material

getGeodeR :: ST BMState ()
getGeodeR =
  do (bp,m,rn) <- stState
     let needed = bp GeodeR
     if (mObsidian m >= mObsidian needed)
       then case getMaterial needed m of
         Nothing -> return ()
         Just m' -> stUpdate (bp,m',rn {rnGeode = rnGeode rn + 1})
       else getObsidianR
       
getObsidianR :: ST BMState ()
getObsidianR =
  do (bp,m,rn) <- stState
     let needed = bp ClayR
     if (mClay m >= mClay needed)
       then case getMaterial needed m of
         Nothing -> return ()
         Just m' -> stUpdate (bp,m',rn {rnObsidian = rnObsidian rn + 1})
       else getClayR
       
getClayR :: ST BMState ()
getClayR =
  do (bp,m,rn) <- stState
     let needed = bp OreR
     case getMaterial needed m of
       Nothing -> getOreR
       Just m' -> stUpdate (bp,m',rn {rnClay = rnClay rn + 1})

getOreR :: ST BMState ()
getOreR =
  do (bp,m,rn) <- stState
     case getMaterial (bp OreR) m of
       Nothing -> return ()
       Just m' -> stUpdate (bp,m',rn {rnOre = rnOre rn + 1})


mUpdate :: Material -> ST BMState ()
mUpdate m = do (bp,_,rn) <- stState
                      stUpdate (bp,m,rn)

-- Strategy: always build a higher robots first
bRun :: Int -> ST BMState Int
bRun 0 = do (_,m,rn) <- stState
            return (mGeode m)
bRun n = do getGeodeR
            (bp,m,rn) <- stState
            let m' = Material {mOre = mOre m + (rnOre rn),
                               mClay = mClay m + (rnClay rn),
                               mObsidian = mObsidian m + (rnObsidian rn),
                               mGeode = mGeode m + (rnGeode rn)}
            mUpdate m'
            bRun (n-1)
            
bGeodes :: Blueprint -> Int
bGeodes bp = stRun (bRun 24) (bp,noMaterial,justOneOreRobot)
-}


bGeodes :: Blueprint -> Int
bGeodes bp = bRun 3 (bp,noMaterial,justOneOreRobot)

                       
part1 :: [Blueprint] -> Int
part1 bps = bGeodes (bps!!0)

-- Part 2

part2 :: [Blueprint] -> Int
part2 _ = 2

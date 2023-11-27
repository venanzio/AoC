-- Advent of Code 2022, day 16
--  Venanzio Capretta

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
  let cave = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 cave))
  -- putStrLn ("Part 2: " ++ show (part2 cave))

-- Parsing the input

data Valve = Valve {
  vOpen :: Bool,
  vFlowRate :: Int,
  vTunnels :: [String]
  }
  deriving Show

type Cave = M.Map String Valve

pValves :: Parser [String]
pValves = do symbol "; tunnels lead to valves"
             someSepStr word ","
          <|>
          do symbol "; tunnel leads to valve"
             v <- word
             return [v]

pData :: Parser (String,Valve)
pData = do symbol "Valve"
           vName <- word
           symbol "has flow rate="
           fRate <- integer
           vs <- pValves
           return (vName, Valve False fRate vs)

pInput :: Parser Cave
pInput = do vs <- pLines pData
            return (M.fromList vs)

-- Part 1

{- Visit the valves with non-zero flow in all possible sequences
   use dijkstra to determine the shortest path to them.
-}

infinite = 30*100

dijkstra :: Cave -> String -> String -> Int
dijkstra cave s t = dijkstra_aux ((s,0):[(v,infinite) | v <- M.keys cave, v /= s])
  where dijkstra_aux queue =
          let (x,d):queue' = sortOn snd queue
              Just v = M.lookup x cave
          in if x==t then d
               else dijkstra_aux [(y, relax d v y dy) | (y,dy) <- queue']
        relax d v y dy = if y `elem` vTunnels v then min (d+1) dy else dy

-- pressure released by visiting valves in a given order

goodValves :: Cave -> [String]
goodValves cave =
  reverse $ map fst $ sortOn (vFlowRate . snd) $ filter (\(_,v) -> vFlowRate v > 0) (M.toList cave) 

--  [(x,vx | (x,v) <- M.toList cave, vFlowRate v /= 0]

-- Pressure released on a given path thwrough "good" valves
--   also returns remaining minutes

pressure :: Cave -> [String] -> (Int,Int)
pressure cave gvs = pressureFrom cave 30 "AA" gvs

pressureFrom :: Cave -> Int -> String -> [String] -> (Int,Int)
pressureFrom cave min x [] = (0,min)
pressureFrom cave min x (y:gvs) =
  let Just yv = M.lookup y cave
      flow = vFlowRate yv
      dist = dijkstra cave x y
      min0 = min - dist - 1
  in if min0 <= 0 then (0,min)
                  else let (p,min1) = pressureFrom cave min0 y gvs
                       in (min0 * flow + p, min1)


-- best route among a list of valves brute force search

bestRoute :: Cave -> [String] -> Int
bestRoute cave gvs = fst $ bestRouteFrom cave 30 "AA" gvs

bestRouteFrom :: Cave -> Int -> String -> [String] -> (Int,[String])
bestRouteFrom cave _ _ [] = (0,[])
bestRouteFrom cave steps x gvs =
      maximum [let steps' = steps - dijkstra cave x y - 1
                   Just yv = M.lookup y cave
                   flow = steps' * vFlowRate yv
                   gvs' = delete y gvs
                   (flow_gvs', gvs'') = bestRouteFrom cave steps' y gvs'
               in if steps' <= 0 then (0,gvs) else (flow + flow_gvs',gvs'')
              | y <- gvs]

-- Greedy algorithm: Always choose the valve that gives the highest pressure releas
-- Gives incorrect answer

greedyRoute :: Cave -> [String] -> (Int,[String])
greedyRoute cave gvs = gRoute cave 30 "AA" gvs 

gRoute :: Cave -> Int -> String -> [String] -> (Int,[String])
gRoute cave min vs [] = (0,[])
gRoute cave min vs gvs =
  let (vt,flow0) = maxF (vFlow cave min vs) gvs
      min0 = min - dijkstra cave vs vt - 1   -- this is computed twice
      gvs0 = delete vt gvs
      (flow1,gvs1) = gRoute cave min0 vt gvs0
  in if min0 <= 0 then (0,[]) else (flow0 + flow1, vt:gvs1)


-- We move from vs to vt and open the valve

vFlow :: Cave -> Int -> String -> String -> Int
vFlow cave min vs vt =
  let steps = dijkstra cave vs vt
      fmin = min - steps -1  -- minutes the valve will be open
      Just valve = M.lookup vt cave
  in fmin * (vFlowRate valve)

  


-- an A*-like algorithm: use as evaluation function the greedy algorithm
-- Not good: incorrect result

type Route = ([String],Int,Int,[String])
-- a route with its pressure release and remaining minutes and unused valves

rPath :: Route -> [String]
rPath (path,_,_,_) = path

rPressure :: Route -> Int
rPressure (_,p,_,_) = p

rMin :: Route -> Int
rMin (_,_,min,_) = min

rUnused :: Route -> [String]
rUnused (_,_,_,gvs) = gvs

pathRoute :: Cave -> [String] -> [String] -> Route
pathRoute cave path gvs =
  let (p,min) = pressure cave path
      (p',_) = gRoute cave min (last path) gvs
  in (gvs,p+p',min,gvs)

evaluateRoute :: Cave -> [String] -> Route
evaluateRoute cave gvs =
  let (gfl,gr) = greedyRoute cave gvs
  in evRoute cave [(v,pathRoute cave [v] (delete v gvs)) | v <- gvs]

evRoute :: Cave -> [(String,Route)] -> Route
evRoute cave [] = error "This shouldn't be empty"
evRoute cave [(_,r)] = r
evRoute cave queue =
  let ((v0,r0),p0) = maxF (\(_,r) -> rPressure r) queue
      queue' = map update (delete (v0,r0) queue)
      update (v,r) = let path = rPath r0 ++ [v]
                         unused = delete v $ rUnused r0
                         r' = pathRoute cave path unused
                     in if rPressure r < rPressure r'
                     then (v,r')
                     else (v,r)
  in evRoute cave queue'
  

-- Inserting valves one at a time?

insertions :: a -> [a] -> [[a]]
insertions x xs = [xs0++x:xs1 | (xs0,xs1) <- [splitAt i xs | i <- [0..length xs]]]

bestInsertion :: Cave -> String -> [String] -> ([String],Int)
bestInsertion cave v vs = maxF (fst . pressure cave) (insertions v vs)

insRoute :: Cave -> [String] -> ([String],Int)
insRoute cave [] = ([],0)
insRoute cave (v:vs) =
  let (bvs,_) = insRoute cave vs
  in bestInsertion cave v bvs

  
          
part1 :: Cave -> Int
part1 cave = snd $ insRoute cave (goodValves cave)

-- Part 2

bestElephant :: Cave -> [String] -> Int
bestElephant cave gvs = bestElephant_aux 9 "AA" 9 "AA" gvs
  where bestElephant_aux _ _ _ _ [] = 0
        bestElephant_aux me mex el elx gvs =
            maximum [let me' = me - dijkstra cave mex y - 1
                         el' = el - dijkstra cave elx y - 1
                         Just yv = M.lookup y cave
                         gvs' = delete y gvs
                     in if me' <= 0 && el' <= 0 then 0
                          else if me' >= el'
                            then (me' * vFlowRate yv) + bestElephant_aux me' y el elx gvs'
                            else (el' * vFlowRate yv) + bestElephant_aux me mex el' y gvs'
                    | y <- gvs]
          
part2 :: Cave -> Int
part2 cave = fst $ bestRouteFrom cave 26 "AA" (goodValves cave) -- bestElephant cave (goodValves cave)

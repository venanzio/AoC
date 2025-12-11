-- Advent of Code 2025, day 11
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
  let g = parseAll pInput input
      ps = paths g
  putStrLn ("Part 1: " ++ show (part1 ps))
  putStrLn ("Part 2: " ++ show (part2 ps))

-- Parsing the input

pData :: Parser (String,[String])
pData = do device <- identifier
           symbol ":"
           outputs <- some identifier
           return (device,outputs)

pInput :: Parser (Graph String)
pInput = do input <- pLines pData
            return (M.fromList (("out",[]):[(d,map (\o -> (o,1)) os)
                                           | (d,os) <- input]))

-- Part 1

paths :: Graph String -> M.Map (String,String) Int
paths g = pathsM where
  devs = M.keys g
  pathsM = M.fromList [((v0,v1), pathsAux v0 v1) | v0 <- devs, v1 <- devs]
  pathsAux v0 v1
    | v0==v1 = 1
    | otherwise = sum $ [M.findWithDefault 0 (w,v1) pathsM
    | (w,_) <- M.findWithDefault undefined v0 g]

part1 :: M.Map (String,String) Int -> Int
part1 ps = M.findWithDefault undefined ("you","out") ps

-- Part 2

part2 :: M.Map (String,String) Int -> Int
part2 ps = M.findWithDefault 0 ("svr", "dac") ps *
           M.findWithDefault 0 ("dac", "fft") ps *
           M.findWithDefault 0 ("fft", "out") ps +
           M.findWithDefault 0 ("svr", "fft") ps *
           M.findWithDefault 0 ("fft", "dac") ps *
           M.findWithDefault 0 ("dac", "out") ps

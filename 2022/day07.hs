-- Advent of Code 2022, day 7
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
  let xs = parseAll pInput input
  putStrLn ("Part 1: " ++ show (part1 xs))
  putStrLn ("Part 2: " ++ show (part2 xs))

-- Parsing the input

type Directory = [String]  -- inverse path
type File = (String,Integer)

type FileSystem = M.Map Directory ([Directory],[File])

pDir :: Directory -> Parser Directory
pDir d = do symbol "dir"
            dirName <- label
            return (dirName:d)

pFile :: Parser File
pFile = do size <- natural
           fileName <- label
           return (fileName,size)

pDirCont :: Directory -> Parser ([Directory],[File])
pDirCont d = do dp <- pDir d
                (ds,fs) <- pDirCont d
                return (dp:ds,fs)
             <|>
             do f <- pFile
                (ds,fs) <- pDirCont d
                return (ds,f:fs)
            <|> return ([],[])

pDirectory :: Directory -> Parser (Directory,[Directory],[File])
pDirectory d = do symbol "$ cd .."
                  pDirectory (tail d)
               <|>
               do symbol "$ cd "
                  dirName <- label
                  symbol "$ ls"
                  (ds,fs) <- pDirCont (dirName:d)
                  return (dirName:d,ds,fs)

pFS :: Directory -> Parser FileSystem
pFS d = do (d',ds,fs) <- pDirectory d
           fsys <- pFS d' 
           return (M.insert d' (ds,fs) fsys)
        <|> return M.empty

pInput :: Parser FileSystem
pInput = pFS []

-- Part 1

type DirSize = M.Map Directory Integer

sumDS :: [Directory] -> DirSize -> Integer
sumDS ds dSize = sum (map (dSize M.!) ds)

dirSize :: FileSystem -> DirSize
dirSize fsys =
  let dsize = M.foldrWithKey
                (\d (ds,fs) ->
                   M.insert d (sumDS ds dsize + sum (map snd fs)))
                M.empty fsys
  in dsize

bigDirSum :: DirSize -> Integer
bigDirSum = sum . filter (<100000) . M.elems

part1 :: FileSystem -> Integer
part1 = bigDirSum . dirSize

-- Part 2

part2 :: FileSystem -> Int
part2 _ = 2

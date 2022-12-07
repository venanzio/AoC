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

type Directory = String
type File = (String,Integer)

type FileSystem = M.Map Directory ([Directory],[File])

pDir :: Parser Directory
pDir = do symbol "dir"
          dirName <- label
          return dirName

pFile :: Parser File
pFile = do size <- natural
           fileName <- label
           return (fileName,size)

pDirCont :: Parser ([Directory],[File])
pDirCont = do d <- pDir
              (ds,fs) <- pDirCont
              return (d:ds,fs)
           <|>
           do f <- pFile
              (ds,fs) <- pDirCont
              return (ds,f:fs)
          <|> return ([],[])

pDirectory :: Parser (Directory,[Directory],[File])
pDirectory = do symbol "$ cd .."
                pDirectory
             <|>
             do symbol "$ cd"
                symbol "ls"
                dirName <- pDir
                (ds,fs) <- pDirCont
                return (dirName,ds,fs)

pFS :: Parser FileSystem
pFS = do (dirName,ds,fs) <- pDirectory
         fsys <- pFS
         return (M.insert dirName (ds,fs) fsys)
      <|> return M.empty

pInput :: Parser FileSystem
pInput = pFS

-- Part 1

part1 :: FileSystem -> Int
part1 _ = 1

-- Part 2

part2 :: FileSystem -> Int
part2 _ = 2

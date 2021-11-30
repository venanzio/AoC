module Main where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M

main :: IO ()
main = do
  putStrLn ("Card key: " ++ show cardKey)
  let cardLoop = lSize cardKey
  putStrLn ("Card loop size: " ++ show cardLoop)
  putStrLn ("Door key: " ++ show doorKey)
  -- let doorLoop = lSize doorKey
  -- putStrLn ("Door loop size: " ++ show doorLoop)
  let enKey = pubKey doorKey cardLoop
  putStrLn ("Encription key: " ++ show enKey)
  return ()

cardKey = 3248366
doorKey = 4738476
  
step :: Int -> Int -> Int
step v subj = v * subj `rem` 20201227
  
pubKey :: Int -> Int -> Int
pubKey subj = pkLoop 1 
  where pkLoop v 0 = v
        pkLoop v n = pkLoop (step v subj) (n-1)

lSize :: Int -> Int
lSize pkey = tryLSize 1 0
  where tryLSize v l = if v==pkey then l else tryLSize (step v 7) (l+1)


module PuzzleInput where

import System.Environment
import Data.List
import Data.Char

import FunParser
import Control.Applicative

import qualified Data.Map as M


puzzle :: String -> IO Int
puzzle fileName = do
  input <- readFile fileName
  let -- xs = map (parseAll expr) (lines input)
      xs = map (parseAll expr2) (lines input)
  return (sum xs)


-- Part 1

expr :: Parser Int
expr = expr' id

expr' :: (Int -> Int) -> Parser Int
expr' f = do me <- (natural <|> parens expr)
             (do symbol "+"
                 expr' (+ (f me))
              <|>
              do symbol "*"
                 expr' (* (f me))
              <|> return (f me))

-- Part 2

expr2 :: Parser Int
expr2 = expr2' id

expr2' :: (Int -> Int) -> Parser Int
expr2' f = do me <- expr2P
              (do symbol "*"
                  expr2' (* (f me))
               <|> return (f me))

expr2P :: Parser Int
expr2P = expr2P' id

expr2P' :: (Int -> Int) -> Parser Int
expr2P' f = do me <- (natural <|> parens expr2)
               (do symbol "+"
                   expr2P' (+ (f me))
                <|> return (f me))


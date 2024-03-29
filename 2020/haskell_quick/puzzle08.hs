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
  let prog = parseAll pProg input
{-
      (Just a) = findLoop $ initSt prog 
  return a
-}
  let prog' = fixProg prog
  return (exec (initSt prog'))
  
data Instr = Acc Int | Jmp Int | Nop Int
  deriving (Eq,Show)

type Prog = [Instr]

pInst :: Parser Instr
pInst = (symbol "acc" >> signed >>= return.Acc) <|>
        (symbol "jmp" >> signed >>= return.Jmp) <|>
        (symbol "nop" >> signed >>= return.Nop)

pProg :: Parser Prog
pProg = some pInst
        
replace :: [a] -> Int -> a -> [a]
replace xs i y = let (xs1,xs2) = splitAt i xs
                     xs2' = if length xs2 == 0 then [y] else y:(tail xs2)
                 in (xs1 ++ xs2')

-- Part 1

data State = State {
  prog :: Prog,
  size :: Int,
  visited :: [Bool],
  acc  :: Int,
  inst :: Int}

initSt :: Prog -> State
initSt pr = State {prog = pr, size = length pr,
                   visited = replicate (length pr) False,
                   acc = 0, inst = 0}


loopCheck :: State -> Maybe Int
loopCheck st =
  case terminate st of
    Nothing -> if (visited st)!!(inst st) then Just (acc st) else Nothing
    _ -> Nothing
    
check :: State -> State
check st = st {visited = replace (visited st) (inst st) True}

goto :: Int -> State -> State
goto n st = st {inst = n}

jump :: Int -> State -> State
jump d st = goto (inst st + d) st

next :: State -> State
next = jump 1

incr :: Int -> State -> State
incr x st = st {acc = acc st + x}

instr :: Instr -> State -> State
instr (Acc d) = next . incr d . check
instr (Jmp n) = jump n . check
instr (Nop x) = next . check

findLoop :: State -> Maybe Int
findLoop st = case loopCheck st of
  Just a -> Just a
  Nothing -> case terminate st of
    Just _ -> Nothing
    Nothing -> findLoop $ instr (prog st !! inst st) st

-- Part 2

terminate :: State -> Maybe Int
terminate st = if inst st >= size st --  (length $ prog st)
               then Just (acc st)
               else Nothing

exec :: State -> Int
exec st = case terminate st of
  Just a -> a
  Nothing -> exec $ instr (prog st !! inst st) st

chInst :: Instr -> Instr
chInst (Jmp d) = Nop d
chInst (Nop d) = Jmp d
chInst i       = i

changeInst :: Prog -> Int -> Prog
changeInst pr n = replace pr n (chInst (pr!!n))

fixProg :: Prog -> Prog
fixProg pr = fixP pr 0

fixP :: Prog -> Int -> Prog
fixP pr n =
  let pr' = changeInst pr n
  in case findLoop (initSt pr') of
       Nothing -> pr'
       Just _  -> fixP pr (n+1)


-- Advent of Code 2023, day 12

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

pData :: Parser (String,[Int])
pData = do row <- token label
           groups <- someSep natural (char ',')
           return (row,groups)

pInput :: Parser [(String,[Int])]
pInput = pLines pData

-- Part 1

arrangements :: String -> [Int] -> Int
arrangements r gs = sum
  [arrangements0 (drop n r) gs |
   n <- [0 .. length (takeWhile (`elem` ".?") r)] ]

arrangements0 :: String -> [Int] -> Int
arrangements0 r [] = if all (`elem` ".?") r then 1 else 0
arrangements0 r [0] = if all (`elem` ".?") r then 1 else 0
arrangements0 r gs | length r < sum gs + length gs - 1 = 0
arrangements0 r (0:gs) =  sum
  [arrangements0 (drop n r) gs |
   n <- [1 .. length (takeWhile (`elem` ".?") r)] ]
arrangements0 r (g:gs) = if length (takeWhile (`elem` "#?") r) >= g
  then arrangements0 (drop g r) (0:gs)
  else 0

part1 :: [(String,[Int])] -> Int
part1 xs = sum [arrangements r gs | (r,gs) <- xs]

-- Part 2

arrangements2 :: String -> [Int] -> Int
arrangements2 r [] = if all (`elem` ".?") r then 1 else 0
arrangements2 r gs | length r < sum gs + length gs - 1 = 0
arrangements2 r gs = arrangeAll r gs -- splitSpring r (delete 0 gs)

splitAtMax :: [Int] -> (Int,[Int],[Int])
splitAtMax gs = let g = maximum gs
                    (gsa,gsb) = break (==g) gs
                in (g,gsa,tail gsb)


groupStart :: String -> [(String,String)]
groupStart r = ("",r) : gStart r where
  gStart r =
    let (ra,rb) = break (`elem` ".?") r
    in if rb == "" then []
       else (ra,tail rb) : map (\(r0a,r0b) -> (ra++(head rb):r0a,r0b))
                               (gStart (tail rb))

isGroup :: Int -> String -> [String]
isGroup g r = let (ra,rb) = splitAt g r in
  if length ra == g && all (`elem` "#?") ra
  then if rb == "" then [""] else
        if head rb `elem` ".?" then [(tail rb)]
          else []
  else []
  
allGroups :: Int -> String -> [(String,String)]
allGroups g r =
  [(ra,rb')  | (ra,rb) <- groupStart r, rb' <- isGroup g rb]

splitString :: String -> Int -> [(String,String)]
splitString r g =  ssFind 0 ("","",r) where
  ssFind n (ra,rc,rb) | n==g =
    ssShift ra rc rb ++
      let (rb1,rb2) = break (=='.') rb
      in if rb2 == "" then [] else ssFind 0 (ra++rc++rb1++".","",tail rb2)
  ssFind n (ra,rc,"") = []
  ssFind n (ra,rc,c:rb) = if c `elem` "#?" then ssFind (n+1) (ra,rc++[c],rb)
                                           else ssFind 0 (ra++rc++[c],"",rb)

  ssShift ra rc "" = [(ra,"")]
  ssShift ra rc (c:rb) = (ra,c:rb) :
    if c `elem` "#?" then ssShift (ra++[head rc]) (tail rc ++ [c]) rb
                     else []

-- number of arrangements of the two splittings above
splitArrange :: ([Int],[Int]) -> (String,String) -> Int
splitArrange (ga,gb) (ra,rb) =
  (arrangements2 ra ga) * (arrangements2 rb gb)

arrangeAll :: String -> [Int] -> Int
arrangeAll r gs = let (g,ga,gb) = splitAtMax gs in
  sum [splitArrange (ga,gb) (ra,rb) | (ra,rb) <- splitString r g]



-- "Optimization" still slow

-- Maximum sequence of consecutive #s
maxSpring :: String -> (Int,String,String)
maxSpring "" = (0,"","")
maxSpring r =
  let r' = takeWhile (/='#') r
      r0 = drop (length r') r
      r0a = takeWhile (=='#') r0
      n0 = length r0a
      r0b = drop n0 r0
      (n1,r1a,r1b) = maxSpring (drop 1 r0b)
  in if n1 > n0 then (n1,r'++r0a++r1a,r1b) else (n0,r',r0b)

-- possible splittings of a group at elements with lower bounded size
splittings :: Int -> [Int] -> [(Int,[Int],[Int])]
splittings n gs = case break (>=n) gs of
  (gs,[]) -> []
  (gsa,(g:gsb)) ->
    (g,gsa,gsb) : [(g',gsa++g:gsa',gsb') | (g',gsa',gsb') <- splittings n gsb]

-- number of arrangements of the two splittings above
extraSpring :: (Int,String,String) -> (Int,[Int],[Int]) -> Int
extraSpring (n,ra,rb) (g,ga,gb) = sum
  [(arrangements2 ra (i:ga)) * (arrangements2 rb (g-n-i:ga))
  | i <- [0..g-n]]

-- For all possible splittings of the group
splitSpring :: String -> [Int] -> Int
splitSpring r gs = case maxSpring r of
  (0,r0,_) -> arrangements r0 gs
  (n,r0a,r0b) -> sum (map (extraSpring (n,r0a,r0b)) (splittings n gs))
    



unfold :: (String,[Int]) ->  (String,[Int])
unfold (r,gs) = (concat (r:take 4 (repeat ('?':r))), concat (take 5 (repeat gs)))

part2 :: [(String,[Int])] -> Int
part2 xs = sum [arrangements2 r gs | (r,gs) <- map unfold xs]

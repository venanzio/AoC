{-
Compilers Course (COMP3012), 2020
  Venanzio Capretta

Functional parsing library
based on chapter 13 of "Programming in Haskell" (2nd edition)
Graham Hutton, Cambridge University Press, 2016.
-}

module FunParser where

import Control.Applicative
import Data.Char

-- Basic definitions

newtype Parser a = P (String -> [(a,String)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) = p

item :: Parser Char
item = P (\inp -> case inp of
                     []     -> []
                     (x:xs) -> [(x,xs)])

-- Returning the result of a deterministic parser that consume all the input
parseAll :: Parser a -> String -> a
parseAll pa src = case (parse pa src) of
  [(a,"")] -> a
  [(a,src')] -> error ("Incomplete parsing: "++src')
  _ -> error ("Parse error")

-- Sequencing parsers

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g pa = P (\src -> [ (g x, src1) | (x,src1) <- parse pa src ])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x = P (\src -> [(x,src)])

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = P (\src -> [ (f x,src2) | (f,src1) <- parse pf src,
                                        (x,src2) <- parse pa src1 ] )

instance Monad Parser where
  -- return :: a -> Parser a
  -- return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  pa >>= fpb = P (\src -> [r | (x,src1) <- parse pa src,
                               r <- parse (fpb x) src1 ] )

--Making choices

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\rsc -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = P (\src -> case parse p1 src of
                    [] -> parse p2 src
                    rs -> rs)

-- Chosing among many alternatives
choice :: Alternative f => [f a] -> f a
choice = foldl (<|>) empty

{-
Parallel parsing: getting the results of both parsers
  for ambiguous grammars
Use with caution: it can cause inefficiency
-}

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
p1 <||> p2 = P (\inp -> (parse p1 inp) ++ (parse p2 inp))

-- Derived primitives

-- verify that the parsed object satisfy a condition
satisfy :: Parser a -> (a -> Bool) -> Parser a
satisfy p cond = do x <- p
                    if cond x then return x else empty

sat :: (Char -> Bool) -> Parser Char
sat = satisfy item

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

label :: Parser String
label = some (sat (not.isSpace))

-- A name for an identifier: alphanumeric string starting with a lower-case letter
ident :: Parser String
ident = do x  <- lower
           xs <- many alphanum
           return (x:xs)

nat :: (Num int, Read int) => Parser int
nat = do xs <- some digit
         return (read xs)

int :: (Num int, Read int) => Parser int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

sigNum :: (Num int, Read int) => Parser int
sigNum = (do char '+'
             n <- nat
             return n) <|>
         (do char '-'
             n <- nat
             return (-n))


-- Handling spacing

space :: Parser ()
space = do many (sat isSpace)
           return ()

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

word :: Parser String
word = token (some alphanum)

identifier :: Parser String
identifier = token ident

natural :: (Num int, Read int) => Parser int
natural = token nat

integer :: (Num int, Read int) => Parser int
integer = token int

signed :: (Num int, Read int) => Parser int
signed = token sigNum


symbol :: String -> Parser String
symbol xs = token (string xs)

-- parsing inside delimiters (like parentheses)
delim :: String -> Parser a -> String -> Parser a
delim left pa right = do
  symbol left
  x <- pa
  symbol right
  return x

parens :: Parser a -> Parser a
parens pa = delim "(" pa ")"

-- parse a sequence of pas separated by ps
seqSep :: Parser a -> String -> Parser [a]
seqSep pa sep = seqSep1 pa sep <|> return []
{-
where seqSep' = do
          x <- pa
          xs <- many (symbol sep >> pa)
          return (x:xs)
-}

-- at least one
seqSep1 :: Parser a -> String -> Parser [a]
seqSep1 pa sep = do
  x <- pa
  xs <- many (symbol sep >> pa)
  return (x:xs)

-- Example: parsing a list of integers
nats :: Parser [Integer]
nats = delim "[" (seqSep natural ",") "]"




-- Parsing blocks of data separated by empty lines

block :: Parser a -> Parser a
block p = P $ \src ->
  case parse chunk src of
    [(ch,src')] -> map (\(a,ch') -> (a,ch'++src')) (parse p ch)
    _ -> []
    
blocks :: Parser a -> Parser [a]
blocks = many . block


-- parse a string until empty line(s) or eof

chunk :: Parser String
chunk = beforeNL <|> pAll

beforeNL :: Parser String
beforeNL = do
  c <- item
  if c=='\n' then afterNL
             else beforeNL >>= return . (c:)

afterNL :: Parser String
afterNL = do
  blanks <- many (char ' ')
  c <- item
  if c=='\n' then (many emptyLine) >> return ""
             else beforeNL >>= return . (("\n" ++ blanks ++ [c]) ++)

-- empty line
emptyLine :: Parser ()
emptyLine = (many (char ' ')) >> char '\n' >> return ()

line :: Parser String
line = (do c <- item
           if c == '\n'
             then return ""
             else line >>= return . (c:))
       <|> pAll


  

-- any non-empty string
pAll :: Parser String
pAll = satisfy (many item) (not . (all isSpace))

  

sepDNL :: Parser [String]
sepDNL = (beforeNL >>= \s -> sepDNL >>= \ss -> return (s:ss))
         <|> (pAll >>= \s -> return [s])


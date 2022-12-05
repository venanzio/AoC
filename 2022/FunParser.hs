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

noItem :: Parser Bool
noItem = P (\inp -> case inp of
               [] -> [(True,[])]
               xs -> [(False,xs)])

-- Returning the result of a deterministic parser that consume all the input
parseAll :: Parser a -> String -> a
parseAll pa src = case (parse pa src) of
 [(a,src')] -> if (allSpace src') then a else error ("Incomplete parsing: "++ src')
 _ -> error ("Parse error")

allSpace :: String -> Bool
allSpace = and . map isSpace

parseFull :: Parser a -> Parser a
parseFull p = P (\inp -> case parse p inp of
                    [(a,"")] -> [(a,"")]
                    _        -> [])

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

parallel :: [Parser a] -> Parser a
parallel = foldl (<||>) empty

someP :: Parser a -> Parser [a]
someP p = do x <- p
             xs <- manyP p
             return (x:xs)

manyP :: Parser a -> Parser [a]
manyP p = someP p <||> return []

-- Parsing a pair
pPair :: Parser a -> Parser b -> Parser (a,b)
pPair p1 p2 = do x1 <- p1
                 x2 <- p2
                 return (x1,x2)

-- Parsing a triple
pTriple :: Parser a -> Parser b -> Parser c -> Parser (a,b,c)
pTriple p1 p2 p3 = do x1 <- p1
                      x2 <- p2
                      x3 <- p3
                      return (x1,x2,x3)

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

-- Parse a specific string
string :: String -> Parser String
string []     = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

-- Parse a sequence of non-space characters
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

-- a sequence of digits, parsed separately
digits :: (Num int, Read int) => Parser [int]
digits = do xs <- some digit
            return (map (\x -> read [x]) xs)

-- signed number
sigNum :: (Num int, Read int) => Parser int
sigNum = (do char '+'
             n <- nat
             return n) <|>
         (do char '-'
             n <- nat
             return (-n))


-- Handling spacing

-- parsing empty space
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


-- Sequences: parsing a sequence of as separated by bs
someSep :: Parser a -> Parser b -> Parser [a]
someSep pa pb = do
  x <- pa
  xs <- many (pb >> pa)
  return (x:xs)

manySep :: Parser a -> Parser b -> Parser [a]
manySep pa pb = someSep pa pb <|> return []


-- When the separator is just a string and we ignore white space
-- at least one
someSepStr :: Parser a -> String -> Parser [a]
someSepStr pa sep = someSep pa (symbol sep)

manySepStr :: Parser a -> String -> Parser [a]
manySepStr pa sep = manySep pa (symbol sep)


-- Example: parsing a list of integers
nats :: Parser [Integer]
nats = delim "[" (manySepStr natural ",") "]"


-- parse p n times
repN :: Int -> Parser a -> Parser [a]
repN 0 p = return []
repN n p = do x <- p
              xs <- repN (n-1) p
              return (x:xs)

-- parsing lines

-- parse a single line
line :: Parser String
line = (do c <- item
           if c == '\n'
             then return ""
             else line >>= return . (c:))
       <|> pAll

-- parse a line with a given parser
pLine :: Parser a -> Parser a
pLine p = do l <- line
             return (parseAll p l)

-- parse many lines (ignoring empty lines)
pLines :: Parser a -> Parser [a]
pLines p = many (token $ pLine p)


-- Parsing blocks of data separated by empty lines

block :: Parser a -> Parser a
block p = P $ \src ->
  case parse chunk src of
    [(ch,src')] -> case parse p ch of
      [(a,ch')] -> if all isSpace ch' then [(a,src')] else []
      _         -> []
    _ -> []

blocks :: Parser a -> Parser [a]
blocks = many . block


-- parse a string until empty line(s) or eof

chunk :: Parser String
chunk = do many emptyLn
           ls <- some neLine
           return (unlines ls)

{-

  do s <- many (sat (/='\n'))
           char '\n'
           if all isSpace s then empty else return s
-}

-- empty line
emptyLine :: Parser ()
emptyLine = (many (char ' ')) >> char '\n' >> return ()



-- non-empty line
neLine :: Parser String
neLine = satisfy line (not . all (isSpace))
  
  
-- next line (skipping new-line characters)
ln :: Parser String
ln = do many (char '\n')
        l <- some (sat (/='\n'))
        (string "\n" <|> return "")
        return l

-- empty line (may be all spaces)
emptyLn :: Parser String
emptyLn = string "\n" <|> satisfy ln (all isSpace)

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


-- any non-empty string
pAll :: Parser String
pAll = satisfy (some item) (not . (all isSpace))

  

sepDNL :: Parser [String]
sepDNL = (beforeNL >>= \s -> sepDNL >>= \ss -> return (s:ss))
         <|> (pAll >>= \s -> return [s])


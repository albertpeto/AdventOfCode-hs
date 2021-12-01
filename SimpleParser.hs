module SimpleParser 
( Parser
, runParser 
, item 
, char
, satisfy
, string
, oneOf
, spaces
, token
, digit
, number
, newLine
, validate
, times
, newline
, whitespace
, (<|>)) where

import Control.Monad
import Control.Applicative
import Data.Char
import GHC.Tuple (Unit (Unit))
import System.Console.Haskeline.History (emptyHistory)

newtype Parser a = Parser { parse :: String -> [(a,String)]}

runParser :: Parser a -> String -> a
runParser a s =
    case parse a s of
        [(res, [])] -> res
        [(res, rem)] -> error $ "Parser did not consume entire stream\n\n" ++ "\n\n" ++ take 100 rem
        s -> error $ "Parser error " ++ (show $ length s)

-- Type classes

instance Functor Parser where
    fmap f p = Parser $ \s -> [(f a, b) | (a,b) <- parse p s]

instance Applicative Parser where
    pure = return
    Parser cs1 <*> Parser cs2 = Parser $ \s -> [(f a, s'') | (f,s') <- cs1 s, (a,s'') <- cs2 s']

instance Monad Parser where
    return a = Parser $ \s -> [(a,s)]
    p >>= f = Parser $ \s -> concatMap (\(a,s') -> parse (f a) s') $ parse p s

instance MonadPlus Parser where
    mzero = Parser $ const []
    mplus p q = Parser $ \s -> parse p s ++ parse q s

instance Alternative Parser where
    empty = Parser $ const []
    p <|> q = Parser $ \s -> 
        case parse p s of
            [] -> parse q s
            res -> res

-- Definitions

item :: Parser Char
item = Parser $ 
    \x -> case x of
       [] -> []
       (x:xs) -> [(x,xs)]

char :: Char -> Parser Char 
char c = satisfy (==c)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then return c else empty

validate :: Parser a -> (a -> Bool) -> Parser a
validate x p = x >>= (\a -> if p a then return a else empty)

string :: String -> Parser String
string [] = return []
string (a:as) = do { satisfy (==a); string as; return (a:as) }

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do { res <- p; spaces; return res }

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int 
number = do 
    sign <- string "-" <|> "" <$ string "+" <|> return ""
    digits <- some digit
    return $ read (sign ++ digits)

newLine :: Parser ()
newLine = do { string "\n"; string "\r" <|> return ""; return () }

times :: Int -> Parser a -> Parser [a]
times 0 _ = return []
times n p = do { a <- p; as <- times (n-1) p; return $ a:as }

newline :: Parser String 
newline = string "\n\r" <|> string "\n"

whitespace :: Parser String
whitespace = string " " <|> newline
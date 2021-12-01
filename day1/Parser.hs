module Day1.Parser (depths) where

import SimpleParser 
import Control.Applicative (many)

depths :: Parser [Int]
depths = many $ do { n <- number; newline; return n } 
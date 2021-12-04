module Day3.Parser (diagnostics) where

import SimpleParser
import Control.Applicative

bit :: Parser Int
bit = 1 <$ char '1' <|> 0 <$ char '0'

diagnostics :: Parser [[Int]]
diagnostics = many $ do { bits <- many bit; newline; return bits }
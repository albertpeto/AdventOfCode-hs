module Adventofcode (solvePuzzle) where

import SimpleParser

solvePuzzle :: (Show b) => Parser a -> String -> (a -> b) -> IO ()
solvePuzzle p puzzle f = do
    contents <- readFile $ "inputs/" ++ puzzle ++ ".txt"
    let parsed = runParser p contents
    print $ f parsed
{-# LANGUAGE LambdaCase #-}
import Adventofcode
import SimpleParser
import Day1.Parser as Parser

solution :: [Int] -> Int
solution ds = let pairs = ds `zip` drop 1 ds
              in length $ filter (\case (a,b) -> b > a) pairs

main = solvePuzzle Parser.depths "puzzle1" solution
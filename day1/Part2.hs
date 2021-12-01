{-# LANGUAGE LambdaCase #-}
import Adventofcode
import SimpleParser
import Day1.Parser as Parser
import Data.List (tails)

slidingWindow :: Int -> [a] -> [[a]]
slidingWindow n as = [take n ts | ts <- take (length as + 1 - n) (tails as)]

solution :: [Int] -> Int
solution ds = let sums  = sum <$> slidingWindow 3 ds
                  pairs = sums `zip` drop 1 sums
              in length $ filter (\case (a,b) -> b > a) pairs

main = solvePuzzle Parser.depths "puzzle1" solution
{-# LANGUAGE LambdaCase #-}
import Adventofcode
import Day3.Parser
import Day3.Model

transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (x:xs) = zipWith (:) x (transpose xs)

solution :: [[Int]] -> Int
solution bits = let gamma = map mostFrequent (transpose bits)
                    epsilon = map leastFrequent (transpose bits)
                 in toNumber gamma * toNumber epsilon

main = solvePuzzle diagnostics "puzzle3" solution
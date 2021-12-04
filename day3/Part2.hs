{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
import Adventofcode
import Day3.Parser
import Day3.Model
import Control.Monad.State

filterHeads :: ([Int] -> Int) -> State [[Int]] Int
filterHeads f = do is <- get 
                   let res  = f (map head is)
                       next = filter ((== res) . head) is
                   put $ map tail next
                   return res

rating :: ([Int] -> Int) -> [[Int]] -> [Int]
rating f is = evalState (replicateM (length (head is)) (filterHeads f)) is

solution :: [[Int]] -> Int 
solution is = let oxygen = rating mostFrequent is
                  co2     = rating leastFrequent is
               in toNumber oxygen * toNumber co2

main = solvePuzzle diagnostics "puzzle3" solution
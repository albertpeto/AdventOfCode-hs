module Day3.Model (mostFrequent, leastFrequent, toNumber) where

import Data.List (sortOn, group, length, last, sort)

mostFrequent :: Eq a => Ord a => [a] -> a
mostFrequent = head 
             . last 
             . sortOn length 
             . group
             . sort

leastFrequent :: Eq a => Ord a => [a] -> a
leastFrequent = head 
              . head 
              . sortOn length 
              . group
              . sort

toNumber :: [Int] -> Int
toNumber = foldl (\a i -> a*2+i) 0
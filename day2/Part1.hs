import Adventofcode
import Day2.Parser

type Coord = (Int,Int)
type Update = Coord -> Coord

update :: Command -> Update
update (Up n) (x,y)      = (x,y-n)
update (Down n) (x,y)    = (x,y+n)
update (Forward n) (x,y) = (x+n,y)

solution :: [Command] -> Int
solution cs = let us    = map update cs 
                  (x,y) = foldl (.) id us (0,0)
              in x * y

main = solvePuzzle commands "puzzle2" solution
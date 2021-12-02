import Adventofcode
import Day2.Parser
import Control.Monad.State

type Coord  = (Int,Int)
data S      = S { aim :: Int, coord :: Coord }
type Update = State S ()

update :: Command -> Update
update (Up n)      = modify (\s -> s { aim = aim s - n })
update (Down n)    = modify (\s -> s { aim = aim s + n })
update (Forward n) = modify (\s -> let (x,y) = coord s
                                   in s { coord = (x+n, y+(aim s)*n)})

solution :: [Command] -> Int
solution cs = let us        = map update cs 
                  state     = foldl (>>) (pure ()) us
                  start     = S 0 (0,0)
                  S _ (x,y) = execState state start
              in x * y

main = solvePuzzle commands "puzzle2" solution
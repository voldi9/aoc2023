import Prelude hiding (Left, Right)
import Debug.Trace
import Data.Maybe
import qualified Data.Heap as Heap
import qualified Data.Map as Map


type Pos = (Int, Int)
type Queue = Heap.MinHeap (Int, State)
type Distances = Map.Map State Int
type Validator = State -> Bool

data Dir = Up | Right | Down | Left | None
  deriving (Read, Show, Eq, Ord)

data Grid = Grid {     plane :: Map.Map Pos Int,
                        exit :: Pos, 
                   validator :: Validator,
                    minSteps :: Int}
  -- deriving (Read, Show, Eq, Ord)

data State = State {   pos :: Pos,
                     steps :: Int,
                       dir :: Dir }
  deriving (Read, Show, Eq, Ord)

notOpposite :: Dir -> Dir -> Bool
notOpposite Down   Up  = False
notOpposite  Up   Down = False
notOpposite Left Right = False
notOpposite Right Left = False
notOpposite   _    _   = True

go :: State -> Dir -> State
go state newDir
  | dir state == newDir = State newPos ((steps state)+1) newDir
  | otherwise           = State newPos        1          newDir
  where
    (x, y) = pos state
    newPos = case newDir of
      Up    -> (x-1, y)
      Down  -> (x+1, y)
      Left  -> (x, y-1)
      Right -> (x, y+1)

goNTimes :: Grid -> Int -> State -> Int -> Dir -> (State, Int)
goNTimes grid n state cost newDir
  | (steps newState) < minSteps grid = goNTimes grid n newState (cost+moveCost) newDir
  -- | dir state == newDir = ((go state newDir), moveCost)
  | otherwise           = (newState, (cost+moveCost)) 
    where
      newState = go state newDir
      moveCost = Map.findWithDefault 999999 (pos newState) (plane grid)

initState :: State
initState = State (0,0) 0 None

initHeap :: Queue
initHeap = Heap.singleton (0, initState)

allDirs :: [Dir]
allDirs = [Up, Right, Down, Left]

extractMin :: Queue -> ((Int, State), Queue)
extractMin = fromJust . Heap.view

parse :: String -> Grid
parse = toGrid . Map.fromList . concat . map remap . zip [0..] . lines
  where
    remap (x, s) = map (\(y, v) -> ((x, y), read [v])) . zip [0..] $ s
    toGrid mp = Grid mp (fst (Map.findMax mp)) undefined 0

updateOne :: Grid -> Int -> Queue -> (State, Int) -> Queue
updateOne grid _ heap (pathNode, pathCost)
 -- | trace ("validator (" ++ show pathNode ++ ") = " ++ show ((validator grid) pathNode)) False = undefined
  | (validator grid) pathNode = heap
updateOne grid curDist heap (pathNode, pathCost) = Heap.insert ((curDist+pathCost), pathNode) heap

updateNeighbors :: Grid -> Queue -> Distances -> Int -> State -> Queue
-- updateNeighbors grid heap curDist curState
--    | trace ("looking up " ++ show (map (goNTimes grid (minSteps grid) curState 0) (filter (notOpposite (dir curState)) allDirs)) ++ "is OOB? " ) False = undefined
updateNeighbors grid heap distances curDist curState = foldl (updateOne grid curDist) heap (neighbors allDirs)
  where
    removeOpposite = filter (notOpposite (dir curState))
    advanceStates = map (goNTimes grid (minSteps grid) curState 0)
    removeVisited = filter (\(state, _) -> Map.notMember state distances)
    neighbors = removeVisited . advanceStates . removeOpposite

updateNode :: Grid -> Queue -> Distances -> Int -> State -> (Queue, Distances)
-- updateNode grid heap distances curDist curState
--   | trace ("looking up " ++ show curState ++ "is OOB? " ) False = undefined
updateNode grid heap distances curDist curState = (heap', distances')
  where
    distances' = Map.insert curState curDist distances
    heap' = updateNeighbors grid heap distances curDist curState

dijkstra :: Grid -> Queue -> Distances -> Distances
dijkstra grid heap distances
  | Heap.null heap                = distances
  | Map.member curState distances = dijkstra grid heap' distances
  | otherwise                     = dijkstra grid heap'' distances'
    where 
      ((curDist, curState), heap') =  extractMin heap
      (heap'', distances') = updateNode grid heap distances curDist curState

findMin :: Grid -> Distances -> Int
findMin grid = minimum . map snd . Map.toList . Map.filterWithKey (\s _ -> pos s == exit grid)

solveUnsafe :: Grid -> Int
solveUnsafe grid = findMin grid $ dijkstra grid initHeap Map.empty

solve1 :: Grid -> Int
-- solve1 (Grid plane exit _ _) = dijkstra (Grid plane exit isValid1 1) initHeap Map.empty
solve1 (Grid plane exit _ _) = solveUnsafe (Grid plane exit isValid1 1)
  where
    isValid1 state = x<0 || y<0 || x>fst exit || y>snd exit || steps state > 3
      where
        (x, y) = pos state

solve2 :: Grid -> Int
--solve2 (Grid plane exit _ _) = dijkstra (Grid plane exit isValid2 4) initHeap Map.empty -- solveUnsafe (Grid plane exit isValid2 4)
solve2 (Grid plane exit _ _) = solveUnsafe (Grid plane exit isValid2 4)
  where
    isValid2 state = x<0 || y<0 || x>fst exit || y>snd exit || steps state > 10
      where
        (x, y) = pos state

main :: IO ()
main = interact ((++"\n") . show . solve2 . parse)
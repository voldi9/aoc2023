import Prelude hiding (Left, Right)
import qualified Data.Vector as V
import Data.Map.Strict (Map, fromList, toList, (!), member, notMember, empty, insert, size)
import Data.Function.Memoize
import Debug.Trace
import Data.List (sort, nub)

type Pos = (Int, Int)
type Grid = Map Pos Char
type Visited = Map State Bool

data Dir = Up | Right | Down | Left
  deriving (Read, Show, Eq, Ord)

data State = State {pos :: Pos,
                    dir :: Dir}
  deriving (Read, Show, Eq, Ord)

go :: State -> State
go (State (x, y) dir) 
  | dir == Up    = State (x-1, y) Up
  | dir == Right = State (x, y+1) Right
  | dir == Down  = State (x+1, y) Down
  | dir == Left  = State (x, y-1) Left

turnSlash :: State -> State
turnSlash (State pos dir) 
  | dir == Up    = State pos Right
  | dir == Right = State pos Up
  | dir == Down  = State pos Left
  | dir == Left  = State pos Down

turnBackslash :: State -> State
turnBackslash (State pos dir) 
  | dir == Up    = State pos Left
  | dir == Right = State pos Down
  | dir == Down  = State pos Right
  | dir == Left  = State pos Up

initState :: State
initState = State (0, 0) Right

parseGrid :: String -> Grid
parseGrid = fromList . concat . zipWith remap [0..] . lines
  where
    remap row line = map (\(column, char) -> ((row, column), char)) $ zip [0..] line

advanceState :: Grid -> State -> [State]
advanceState grid state = map go $ case grid ! (pos state) of
      '.' -> [state]
      '-' -> if elem (dir state) [Left, Right] then [state] else [State (pos state) Left, State (pos state) Right]
      '|' -> if elem (dir state) [Up, Down] then [state] else [State (pos state) Up, State (pos state) Down]
      '/' -> [turnSlash state]
      '\\'-> [turnBackslash state]

advanceAllStates :: Visited -> [State] -> Grid -> Visited
advanceAllStates visited   []   grid = visited
advanceAllStates visited states grid = advanceAllStates visited' states' grid
  where
    visited' = foldl (\m s -> insert s True m) visited states
    states' = filter outOfBoundsOrVisited . concat . map (advanceState grid) $ states
    outOfBoundsOrVisited x = notMember x visited' && member (pos x) grid

findMaxXY :: Grid -> (Int, Int)
findMaxXY grid = (maximum (map fst ps), maximum (map snd ps))
  where
    ps = (map fst . toList) grid

mapSize :: Visited -> Int
mapSize = length . nub . sort . map pos . map fst . toList

solve :: Grid -> Int
solve = mapSize . advanceAllStates empty [initState]

initStates2 :: Grid -> [State]
initStates2 grid = top ++ right ++ bottom ++ left
  where
    (mX, mY) = findMaxXY grid
    top    = [State (0, y) Down | y <- [0..mY]]
    right  = [State (x, mY) Left | x <- [0..mX]]
    bottom = [State (mX, y) Up | y <- [0..mY]]
    left   = [State (x, 0) Right | x <- [0..mX]]

solve2 :: Grid -> Int
solve2 grid = maximum . map mapSize . map advanceInitState $ (initStates2 grid)
  where
    advanceInitState state = advanceAllStates empty [state] grid

main :: IO ()
main = interact ((++"\n") . show . solve2 . parseGrid)
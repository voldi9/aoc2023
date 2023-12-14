import Aoc
import Data.List.Split
import Data.Char
import Data.Maybe
import Data.Function.Memoize

import Data.Set (Set, fromList, insert, union, member)
import qualified Data.Map as Map
import Data.List (tails, sortBy)
import Control.Monad (when)

type Pos = (Int, Int)
type Move = (Pos, Dir)
type Grid = Map.Map Pos Char
type DistMap = Map.Map Pos Int
type ConnectMap = Map.Map Pos Bool

data Dir = North | East | South | West | None deriving (Eq, Show)

isValidTurn :: Dir -> Char -> Bool
isValidTurn d c = turn d c /= None

turn :: Dir -> Char -> Dir
turn South 'L' = East
turn South 'J' = West
turn South '|' = South
turn South _   = None
turn North 'F' = East
turn North '7' = West
turn North '|' = North
turn North _   = None
turn East  'J' = North
turn East  '7' = South
turn East  '-' = East
turn East  _   = None
turn West  'F' = South
turn West  'L' = North
turn West  '-' = West
turn West  _   = None

followPath :: Grid -> Move -> Move 
followPath g mv@((x,y), dir)
  | dir == North && g Map.! (x,y) == 'F' = ((x, y+1), East)
  | dir == North && g Map.! (x,y) == '7' = ((x, y-1), West)
  | dir == North && g Map.! (x,y) == '|' = ((x-1, y), North)
  | dir == South && g Map.! (x,y) == 'L' = ((x, y+1), East)
  | dir == South && g Map.! (x,y) == 'J' = ((x, y-1), West)
  | dir == South && g Map.! (x,y) == '|' = ((x+1, y), South)
  | dir == West  && g Map.! (x,y) == 'F' = ((x+1, y), South)
  | dir == West  && g Map.! (x,y) == 'L' = ((x-1, y), North)
  | dir == West  && g Map.! (x,y) == '-' = ((x, y-1), West)
  | dir == East  && g Map.! (x,y) == '7' = ((x+1, y), South)
  | dir == East  && g Map.! (x,y) == 'J' = ((x-1, y), North)
  | dir == East  && g Map.! (x,y) == '-' = ((x, y+1), East)
  | otherwise = ((x, y), None)

startingPos :: Grid -> Pos
startingPos = fst . Map.findMin . Map.filter (=='S')

directions :: [Move]
directions = zip (zipWith pair [1, 0, -1, 0] [0, -1, 0, 1]) [South, West, North, East]
  where
    pair a b = (a, b)

moves :: Move -> [Move]
moves mv@((x, y), _) = map f directions
  where
    f ((dx, dy), dir) = ((x + dx, y + dy), dir)

isWithinBounds :: Grid -> Move -> Bool
isWithinBounds g (p, _) = isPosWithinBounds g p

isPosWithinBounds :: Grid -> Pos -> Bool
isPosWithinBounds g (x, y) = x >= 0 && y >= 0 && x < 140 && y < 140

neighbors :: Grid -> Move -> [Move]
neighbors g = filter (isWithinBounds g) . moves

isValidMove :: Grid -> Move -> Bool
isValidMove g m@(pos, dir) = isValidTurn dir (g Map.! pos)  

transform :: Grid -> DistMap -> [Move] -> [Move]
transform g m = filter (not . visited . fst) . concat . map (neighbors g)
  where
    visited p = Map.member p m

distMap :: Int -> [Move] -> DistMap
distMap d l = Map.fromList [(p, d) | p <- map fst l]

computeDist :: Grid -> DistMap
computeDist g = traverseGrid g 1 initDistMap [(sPos, None)]
  where
    sPos = startingPos g
    initDistMap = Map.fromList [(sPos, 0)]

traverseGrid :: Grid -> Int -> DistMap -> [Move] -> DistMap
traverseGrid _ _ m [] = m
traverseGrid g d m l  = traverseGrid g (d+1) (newMap `Map.union` m) newMoves
  where
    newMoves = filter (isValidMove g) (transform g m l)
    newMap = distMap d newMoves

parse :: String -> Grid
parse = Map.fromList . concat . zipWith remap [0..] . lines
  where
    remap row line = map (\(column, char) -> ((row, column), char)) $ zip [0..] line

solve :: String -> Int
solve = maximum . map snd . Map.toList . computeDist . parse 

-- Used for shoelace formula
determinant :: (Pos, Pos) -> Int 
determinant ((x1, y1), (x2, y2)) = x1*y2 - x2*y1

totalDeterminant :: [Pos] -> Int
totalDeterminant ps = sum $ map determinant $ zip (drop 1 ps) ps

dfsPointList :: Grid -> DistMap -> [Pos] -> Move -> [Pos]
dfsPointList g dm ps mv@(p, dir)
  | Map.notMember p dm || dir == None = []
  | g Map.! p == 'S'                  = p:ps
  | otherwise                         = dfsPointList g dm (p:ps) (followPath g mv)

solve2 :: String -> Int
solve2 s = ((totalDeterminant points) - (length points) + 3) `div` 2
  where
    grid = parse s
    dist = computeDist grid
    sPos@(x,y) = startingPos grid
    points = (dfsPointList grid dist [] firstMove) ++ [sPos]
    firstMove
      | Map.member (x+1, y) grid && elem (grid Map.! (x+1, y)) ['|', 'F', '7'] = ((x+1, y), South)
      | Map.member (x-1, y) grid && elem (grid Map.! (x-1, y)) ['|', 'J', 'L'] = ((x-1, y), North)
      | Map.member (x, y-1) grid && elem (grid Map.! (x, y-1)) ['-', 'F', 'L'] = ((x, y-1), West)
      | Map.member (x, y+1) grid && elem (grid Map.! (x, y+1)) ['-', 'J', '7'] = ((x, y+1), East)

main :: IO ()
main = interact (show . solve2)

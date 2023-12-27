import Debug.Trace
import Data.List (nub, sort, (\\))
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set

type Pos = (Int, Int)
type Edge = (Int, Pos)
type Visited = Set.Set Pos
data Grid = Grid { grid :: Map.Map Pos Char, startPos, endPos :: Pos }
type Graph = Map.Map Pos [Edge]

constructGrid :: Map.Map Pos Char -> Grid
constructGrid mp = Grid mp (fst (Map.findMin mp)) (fst (Map.findMax mp)) 

neighbors :: Grid -> Pos -> [Pos]
neighbors g (x,y) = filter ((flip Map.member) (grid g)) $ case (grid g) Map.! (x,y) of 
    '.' -> [(x+1,y), (x-1, y), (x, y+1), (x, y-1)]
    '>' -> [(x, y+1)]
    '<' -> [(x, y-1)]
    'v' -> [(x+1, y)]
    '^' -> [(x-1, y)]

neighbors2 :: Grid -> Pos -> [Pos]
neighbors2 g (x,y) = filter ((flip Map.member) (grid g)) [(x+1,y), (x-1, y), (x, y+1), (x, y-1)]

parseGrid :: String -> Grid
parseGrid = constructGrid . Map.fromList . filter ((/='#') . snd) . concat . zipWith remap [0..] . lines
  where
    remap row line = map (\(column, char) -> ((row, column), char)) $ zip [0..] line

advance :: (Pos -> [Pos]) -> Int -> Pos -> Pos -> Edge
advance nsfg c p p'
  | length (nsfg p') == 2 = advance nsfg (c+1) p' (head ((nsfg p')\\[p]))
  | otherwise = (c+1, p')

neighborDist :: (Pos -> [Pos]) -> [Pos] -> Pos -> [Edge]
neighborDist nsfg ns p = map (advance nsfg 0 p) ns

reverseEdges :: Graph -> Graph
reverseEdges g = Map.map (nub . sort) $ foldl insertReverse g (Map.toList g)
  where
    insertReverse g' (p,ns) = adjust g' p ns
    adjust g' p ns = foldl (\g'' (c,p') -> Map.insertWith (++) p' [(c,p)] g'') g' ns

constructGraph :: (Pos -> [Pos]) -> [Pos] -> Graph -> Graph
constructGraph  _     []   gr = reverseEdges gr
constructGraph nsfg (p:ps) gr = constructGraph nsfg 
                                               ((map snd unvisitedAdvanced)++ps) 
                                               (Map.insert p advanced gr)
  where advanced = neighborDist nsfg (nsfg p) p
        unvisitedAdvanced = filter (((flip Map.notMember) gr) . snd) advanced

dfs :: (Pos -> [Pos]) -> Pos -> Graph -> Visited -> Pos -> Int
dfs nsfg endPos gr vis p
  | null ns = if p == endPos then 0 else -1000000
  | otherwise = maximum (map (\(c, p') -> c + dfs nsfg endPos gr (Set.insert p vis) p') ns)
    where ns = filter (((flip Set.notMember) vis) . snd) (gr Map.! p)

solve :: (Grid -> Pos -> [Pos]) -> Grid -> Int
solve nsf g = dfs (nsf g) 
                  (endPos g) 
                  (constructGraph (nsf g) [startPos g] Map.empty)
                  (Set.singleton (startPos g))
                  (startPos g)

solve1 :: Grid -> Int
solve1 = solve neighbors

solve2 :: Grid -> Int
solve2 = solve neighbors2

main :: IO ()
main = interact ((++"\n") . show . solve2 . parseGrid)

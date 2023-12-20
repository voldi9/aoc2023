import Prelude hiding (Left, Right)
import Debug.Trace
import Data.Maybe
import Data.List (nub, (\\))
import Data.List.Split (splitOn)
import Numeric (readHex)
import qualified Data.Map as Map

type Pos = (Integer, Integer)
type ColumnMap = Map.Map Integer [Integer]

data State = State { pos :: Pos,
                     dir :: Dir}

data Dir = U | R | D | L
  deriving (Read, Show, Eq, Ord)

data Trench = Trench {  points :: [Pos],
                       minX, minY, maxX, maxY :: Integer }
  deriving (Read, Show, Eq, Ord)

initState :: State
initState = State (0,0) R

go :: State -> State
go = goN 1

goN :: Integer -> State -> State
goN n (State (x,y) dir) = State newPos dir
  where 
    newPos = case dir of
        U -> (x-n, y)
        D -> (x+n, y)
        L -> (x, y-n)
        R -> (x, y+n)

mapDir :: Char -> Dir
mapDir '0' = R
mapDir '1' = D
mapDir '2' = L
mapDir '3' = U

combine :: [Pos] -> (State, [Pos]) -> (State, [Pos])
combine l' (s, l) = (s, l ++ l')

digFurther :: (State, [Pos]) -> Integer -> (State, [Pos]) 
digFurther (state, list) 0 = (state, list)
digFurther (state, list) n = combine [pos state] (digFurther ((go state), list) (n-1))

parseLine1 :: (State, [Pos]) -> String -> (State, [Pos])
parseLine1 (state, list) line = combine list (digFurther (state', []) steps)
  where
    [dirString, stepsString, colorString] = splitOn " " line
    state' = State (pos state) (read dirString)
    steps = read stepsString

parseLine2 :: (State, [Pos]) -> String -> (State, [Pos])
parseLine2 (state, list) line = (state', (pos state'):list)
  where
    colorString = tail . tail . init . last $ splitOn " " line
    dir = mapDir (last colorString)
    steps = fst . head $ readHex (init colorString)
    state' = goN steps (State (pos state) dir)


traverseColumns :: ColumnMap -> [Pos] -> ColumnMap
traverseColumns cmap       []         = cmap
traverseColumns cmap (p'@(x', y'):ps) = traverseColumns (Map.adjust (++[x']) y' cmap) ps

parseList :: ((State, [Pos]) -> String -> (State, [Pos])) -> String -> [Pos]
parseList f = snd . foldl f (initState, []) . lines

prepareList :: [Pos] -> [Pos] 
prepareList list
  | snd (last list) == snd (head list) = prepareList ((last list):(init list))
  | otherwise = list

elideColinear :: [Pos] -> Integer -> [Pos] 
elideColinear    [p']      _  = [p']
elideColinear (p':p'':ps') y' =
  if (snd p'') == y' then elideColinear (p'':ps') y' else (p':p'':ps')

groupUp :: [(Pos, Pos)] -> [Pos] -> [(Pos, Pos)]
groupUp cur   []   = reverse cur
groupUp cur (p:ps) = groupUp (toAdd:cur) (tail rest)
  where
    rest = elideColinear (p:ps) (snd p)
    toAdd = if p == head rest then (p, p) else (head rest, p)

filterSides :: [Pos] -> [(Pos, Pos)] -> Integer -> [Pos]
filterSides cur             (p:[])            _     = cur
filterSides cur (p@(p1,p2):p'@(p1',p2'):ps) prevY
  | ((snd p1) - prevY) * ((snd p1') - (snd p1)) < 0 = filterSides    cur   (p':ps) (snd p1)
  | otherwise                                       = filterSides (p1:cur) (p':ps) (snd p1)

countPoints :: Integer -> [Pos] -> ColumnMap -> Integer
countPoints c      []        _  = c
countPoints c (p@(x,y):ps) cmap
  | elem x higherPoints         = countPoints  (c)  ps cmap
  | (odd . length) higherPoints = countPoints (c+1) ps cmap
  | otherwise                   = countPoints  (c)  ps cmap
    where 
      higherPoints = filter (\x' -> x' <= x) (cmap Map.! y) 

determinant :: (Pos, Pos) -> Integer 
determinant ((x1, y1), (x2, y2)) = x1*y2 - x2*y1

totalDeterminant :: [Pos] -> Integer
totalDeterminant ps = abs . sum . map determinant $ zip (drop 1 ps') ps'
  where
    ps' = ((last ps):ps)

solve1 :: String -> Integer
solve1 s = (fromIntegral $ length points) + (countPoints 0 testPoints cmap)
  where
    points = parseList parseLine1 s
    minX = (minimum . map fst) points 
    maxX = (maximum . map fst) points 
    minY = (minimum . map snd) points
    maxY = (maximum . map snd) points 
    groupedPoints = (groupUp [] . prepareList) points
    filteredPoints = filterSides [] (groupedPoints ++ [head groupedPoints]) ((snd . snd . last) groupedPoints)
    cmap = traverseColumns (Map.fromList [(y, []) | y <- [minY..maxY]]) filteredPoints 
    testPoints = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]] \\ points

howManyPoints :: [Pos] -> Integer
howManyPoints ps = hmpHelper 0 ((last ps):ps)
  where
    hmpHelper c         [p]             = c
    hmpHelper c (p@(x,y):p'@(x',y'):ps) = hmpHelper (c+diff) (p':ps)
      where
        diff = abs(y'-y) + abs(x'-x)

solve2 :: String -> Integer
solve2 s = interiorPoints + totalPoints -- ((- (fromIntegral $ length points) + 3) `div` 2 --zip ps' (drop 1 ps')
  where
    points = parseList parseLine2 s
    totalPoints = howManyPoints points
    doubleArea = totalDeterminant points
    interiorPoints = ((doubleArea - totalPoints) `div` 2) + 1

main :: IO ()
main = interact ((++"\n") . show . solve2)

import Debug.Trace
import Data.List (nub, sort)
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set

type Pos = (Int, Int)
type Points = Set.Set Pos
type Moves = Map.Map (Int, Pos) (Points)

data OddEven = OddEven { oddS, evenS :: Points  }
  deriving (Show, Read, Eq, Ord)
data Garden = Garden { pos :: Points, startPos :: Pos, allPos :: [Pos], maxX,maxY :: Int }
  deriving (Show, Read, Eq, Ord)

neighbors :: Garden -> Pos -> [Pos]
neighbors garden (x, y) = filter ((flip Set.member) (pos garden)) ns
  where
    ns = [(x+1,y), (x-1, y), (x, y+1), (x, y-1)]

constructGarden :: [(Pos, Char)] -> Garden
constructGarden points = Garden (Set.fromList (map fst points)) 
                                ((fst . head . filter ((=='S') . snd)) points) 
                                (map fst points)
                                ((maximum . map fst . map fst) points)
                                ((maximum . map snd . map fst) points)

parse :: String -> Garden
parse = constructGarden . concat . map remap . zip [0..] . lines
  where
    remap (x, s) = map (\(y, v) -> ((x, y), v)) . filter (walkable . snd) . zip [0..] $ s
    walkable c = c == '.' || c == 'S'

getReachable :: Pos -> Int -> Garden -> Int
getReachable p step garden = Set.size . evenOrOdd step $ getReachableHelper 
                                                          True
                                                           garden
                                                           step 
                                                           (OddEven (Set.singleton p) (Set.empty))
  where
    evenOrOdd c eos = if even c then oddS eos else evenS eos

getReachableHelper :: Bool -> Garden -> Int -> OddEven -> OddEven
getReachableHelper   _      _     0  oes = oes
getReachableHelper True  garden step oes = getReachableHelper False garden (step-1) $ 
                                           foldl (updateSets garden True) oes (Set.toList (oddS oes))
getReachableHelper False garden step oes = getReachableHelper True garden (step-1) $ 
                                           foldl (updateSets garden False) oes (Set.toList (evenS oes))

updateSets :: Garden -> Bool -> OddEven -> Pos -> OddEven
updateSets garden True  oddEven p = oddEven { evenS = foldl maybeAdd (evenS oddEven) (neighbors garden p) }
updateSets garden False oddEven p = oddEven { oddS = foldl maybeAdd (oddS oddEven) (neighbors garden p) }

maybeAdd :: Points -> Pos -> Points
maybeAdd s p
  | Set.member p s = s
  | otherwise      = Set.insert p s

-- Use: solve 64 . parse
solve :: Int -> Garden -> Int
solve n g = getReachable (startPos g) n $ g

-- -- Use: solve2 26501365 . parse
solve2 :: Int -> Garden -> Int
solve2 n g = (grids*(botRight' + botLeft' + topRight' + topLeft'))
             + ((grids+1)*(botRight + botLeft + topRight + topLeft))
             + topC + rightC + bottomC + leftC
             + (evenGrids*evenNum)
             + (oddGrids*oddNum)
  where
    width = (maxX g) + 1
    grids = (n `div` width) - 1 
    oddGrids = grids * grids
    evenGrids = (grids+1) * (grids+1)
    oddNum -- = 7496
      | odd width  = solve   width   g
      | even width = solve (width+1) g
    evenNum -- = 7570
      | odd width  = solve (width+1) g
      | even width = solve (width+2) g
    (x, y) = startPos g
    diag = ((width+1) `div` 2) - 1
    diag' = ((width*3) `div` 2) - 1
    topC = getReachable (width-1, y) (width-1) g
    rightC = getReachable (x, 0) (width-1) g
    bottomC = getReachable (0, y) (width-1) g
    leftC = getReachable (x, width-1) (width-1) g
    topLeft = getReachable (width-1, width-1) diag g
    topRight = getReachable (width-1, 0) diag g
    botLeft = getReachable (0, width-1) diag g
    botRight = getReachable (0, 0) diag g
    topLeft' = getReachable (width-1, width-1) diag' g
    topRight' = getReachable (width-1, 0) diag' g
    botLeft' = getReachable (0, width-1) diag' g
    botRight' = getReachable (0, 0) diag' g

main :: IO ()
main = interact ((++"\n") . show . solve2 26501365 . parse)

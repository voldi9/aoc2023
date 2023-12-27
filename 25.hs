import Debug.Trace
import System.Random.Stateful
import Data.List ((\\), sort)
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)

type Node = (String, [WeightEdge])
type Edge = (String, String)
type EdgeMap = Map.Map String [WeightEdge]
type EdgeSet = Map.Map Edge Int
type WeightEdge = (String, Int)
type CountMap = Map.Map String Int
data Graph = Graph { edges :: EdgeMap, set :: EdgeSet, count :: CountMap }
  deriving (Show, Read, Eq, Ord)

parseNode :: String -> Node
parseNode s = (name, map (\x -> (x,1)) $ splitOn " " (tail edges))
  where [name, edges] = splitOn ":" s

reverseEdgeSingle :: EdgeMap -> Node -> EdgeMap
reverseEdgeSingle mp (_,   [])       = mp
reverseEdgeSingle mp (v, ((e,c):es)) = reverseEdgeSingle (Map.insertWith (++) e [(v,c)] mp) (v, es)

reverseEdges :: EdgeMap -> EdgeMap
reverseEdges mp = foldl reverseEdgeSingle mp (Map.toList mp)

constructGraph :: EdgeMap -> Graph
constructGraph mp = Graph mp 
                          (Map.fromList . concat $ map mapToEdges (Map.toList mp)) 
                          (Map.fromList $ map mapToCount (Map.toList mp)) 
  where mapToEdges (v, es) = [((v,u),1) | u <- map fst es]
        mapToCount (v, _ ) = (v,1)

parse :: String -> Graph
parse = constructGraph . reverseEdges . Map.fromList . map parseNode . lines

addOrInc :: String -> Int -> [WeightEdge] -> [WeightEdge]
addOrInc v c' es
  | oldCost == 0 = ((v,c'):es)
  | otherwise    = ((v,c'+oldCost):(es\\[head maybeItem]))
  where
    maybeItem = filter ((==v) . fst) es
    oldCost = if null maybeItem then 0 else snd (head maybeItem)

delAddMap :: String -> String -> EdgeMap -> (String, Int) -> EdgeMap
delAddMap v u mp (u',c) = Map.adjust (filter ((/=u) . fst)) u' mp'
  where 
    mp' = if u' == v then mp else
                          (Map.adjust (addOrInc v c) u' . Map.adjust (addOrInc u' c) v) mp
adjustInsert :: String -> String -> Int -> EdgeSet -> EdgeSet
adjustInsert u v c s
  | u == v    = s
  | otherwise = (Map.insertWith (+) (v,u) c . Map.insertWith (+) (u,v) c) s

delAddSet :: String -> String -> EdgeSet -> (String, Int) -> EdgeSet
delAddSet v u st (u',c) = (Map.delete (u',u) . Map.delete (u,u')) (adjustInsert u' v c st)

merge :: Graph -> Edge -> Graph
merge (Graph mp set cnt) (v,u) = Graph mp' set' cnt'
  where
    !neighbors = mp // u
    mp' =  foldl (delAddMap v u) (Map.delete u mp) neighbors
    set' = foldl (delAddSet v u)        set        neighbors
    cnt' = (Map.delete u . Map.insertWith (+) v (cnt Map.! u)) cnt

getRandomEdge :: Graph -> IO Edge
getRandomEdge (Graph _ st _) = do
  i <- uniformRM (0, Map.size st - 1) globalStdGen
  return (fst ((Map.toList st) !! i))

lowestEdge :: Graph -> Edge
lowestEdge (Graph _ st _) = snd . last . sort . map (\(x,y) -> (y,x)) . Map.toList $ st

shrink :: Graph -> IO Graph
shrink g
  | Map.size (set g) == 2 = return g
  | otherwise = do
      edge <- getRandomEdge g 
      shrink (merge g edge)

solve :: Graph -> IO (Graph, [Int])
solve g = do 
  solved <- shrink g
  case Map.findMin (set solved) of
    (_,3) -> return (solved, map snd (Map.toList (count solved)))
    _     -> do
      putStrLn $ show solved
      solve g


main :: IO ()
main = do 
  input <- getContents 
  let graph = parse input
  result <- solve graph
  putStrLn $ show result

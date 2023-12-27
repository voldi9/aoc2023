import Debug.Trace
import Data.List (nub, sort, (\\))
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set

type PointSet = Map.Map Point Int
type HeightMap = Map.Map (Int, Int) (Int, Int)
type BrickMap = Map.Map Int Brick
data Point = Point { x,y,z :: Int }
  deriving (Read, Ord, Eq)
data Brick = Brick Point Point
  deriving (Read, Eq)

instance Show Point where
  show (Point x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

instance Ord Brick where
  compare b1 b2 = compare (getBottom b1) (getBottom b2)

instance Show Brick where
  show (Brick (Point x1 y1 z1) (Point x2 y2 z2)) = "Brick x=("++show lx++"-"++show rx
                                                    ++"), y=("++show by++"-"++show ty
                                                    ++"), z=("++show lz++"-"++show hz++")"
    where
      lx = min x1 x2
      rx = max x1 x2
      ty = max y1 y2
      by = min y1 y2
      lz = min z1 z2
      hz = max z1 z2

getBottom :: Brick -> Int
getBottom (Brick (Point _ _ z1) (Point _ _ z2)) = min z1 z2

getTop :: Brick -> Int
getTop (Brick (Point _ _ z1) (Point _ _ z2)) = max z1 z2

getLeft :: Brick -> Int 
getLeft (Brick (Point x1 _ _) (Point x2 _ _)) = min x1 x2

getRight :: Brick -> Int 
getRight (Brick (Point x1 _ _) (Point x2 _ _)) = max x1 x2

getBehind :: Brick -> Int 
getBehind (Brick (Point _ y1 _) (Point _ y2 _)) = min y1 y2

getFront :: Brick -> Int 
getFront (Brick (Point _ y1 _) (Point _ y2 _)) = max y1 y2

getHeight :: Brick -> Int
getHeight b1 = 1 + (getTop b1 - getBottom b1)

getPoints :: Brick -> [Point]
getPoints b = [ Point x y z | x <- [getLeft b..getRight b], 
                              y <- [getBehind b..getFront b], 
                              z <- [getBottom b..getTop b]]

pointFromStr :: String -> Point
pointFromStr s = Point x y z
  where [x,y,z] = map read (splitOn "," s)

parseBrick :: String -> Brick
parseBrick s = Brick (pointFromStr p1) (pointFromStr p2)
  where [p1,p2] = splitOn "~" s

parse :: String -> [Brick]
parse = sort . map parseBrick . lines

findBelowPoints :: Brick -> [Point]
findBelowPoints b = map (\(x,y) -> Point x y h) (getBottomPoints b)
  where h = (getBottom b) - 1

findAbovePoints :: Brick -> [Point]
findAbovePoints b = map (\(x,y) -> Point x y h) (getBottomPoints b)
  where h = (getTop b) + 1

getBottomPoints :: Brick -> [(Int, Int)]
getBottomPoints (Brick (Point x y z) (Point x' y' z'))
  | x /= x' = [(x'',y) | x'' <- [(min x x')..(max x x')]]
  | y /= y' = [(x,y'') | y'' <- [(min y y')..(max y y')]]
  | otherwise = [(x,y)]

mapLookup :: HeightMap -> (Int, Int) -> (Int, Int)
mapLookup hm p = Map.findWithDefault (0, 0) p hm

findDropHeight :: HeightMap -> Brick -> Int
findDropHeight hm b = (+1) . maximum . map (fst . mapLookup hm) $ (getBottomPoints b)

dropTo :: Int -> Brick -> Brick
dropTo height b@(Brick p1 p2) = Brick (p1 { z = (z p1) - diff}) (p2 { z = (z p2) - diff})
  where diff = (getBottom b) - height 

putHeightOnMap :: HeightMap -> Brick -> HeightMap
putHeightOnMap hm b = foldl mapInsert hm (getBottomPoints b)
  where 
    mapInsert :: HeightMap -> (Int, Int) -> HeightMap
    mapInsert m p = Map.insert p (getTop b, getHeight b) m

putInSet :: PointSet -> Brick -> PointSet
putInSet s b = foldl insertWithId s (getPoints b)
  where
    !iden = Map.size s
    insertWithId s' p = Map.insert p iden s'

putOnMap :: PointSet -> BrickMap -> Brick -> BrickMap
putOnMap ps m b = Map.insert iden b m
  where
    !iden = ps Map.! (head (getPoints b))

dropBrick :: (HeightMap, [Brick]) -> Brick -> (HeightMap, [Brick])
dropBrick (hm, bs) b = (hm', (b':bs))
  where b' = dropTo (findDropHeight hm b) b
        hm' = putHeightOnMap hm b'

simulateDrops :: [Brick] -> (HeightMap, [Brick])
simulateDrops bricks = (\(mp, bs) -> (mp, sort bs)) $ foldl dropBrick (Map.empty, []) bricks

fillPointSet :: (HeightMap, [Brick]) -> (PointSet, BrickMap, [Brick])
fillPointSet (hm, bs) = (ps, mp, bs)
  where
    ps = foldl   putInSet    Map.empty bs
    mp = foldl (putOnMap ps) Map.empty bs

isLyingOn :: PointSet -> Brick -> [Int]
isLyingOn ps b = nub . sort . filter (/=(-1)) . map (findOnMap ps) $ (findBelowPoints b)
  where findOnMap = flip (Map.findWithDefault (-1))

areLyingOnMe :: PointSet -> Brick -> [Int]
areLyingOnMe ps b = nub . sort . filter (/=(-1)) . map (findOnMap ps) $ (findAbovePoints b)
  where findOnMap = flip (Map.findWithDefault (-1))

countNotSupporting :: (PointSet, BrickMap, [Brick]) -> Int
countNotSupporting (ps, _, bs) = (length bs) - (Set.size (taintedSet bs))
  where
    thoseWithSingleSupport = map head . filter ((==1) . length) . map (isLyingOn ps)
    taintedSet = foldl (flip Set.insert) Set.empty . thoseWithSingleSupport

disintegrate :: PointSet -> Brick -> PointSet
disintegrate ps b = foldl (flip Map.delete) ps (getPoints b)

maybeDisintegrate :: (PointSet, [Brick], Int) -> Brick -> (PointSet, [Brick], Int)
maybeDisintegrate (ps, bs, c) b
  | length memberPts > 0 && all (==False) memberPts = (disintegrate ps b, bs\\[b], c+1) 
  | otherwise                                       = (ps,                bs,      c)
    where memberPts = map ((flip Map.member) ps) (filter ((/=0) . z) (findBelowPoints b))

chainReaction :: PointSet -> [Brick] -> Int
chainReaction ps bs = extractCount $ foldl maybeDisintegrate (ps, bs, 0) bs
  where extractCount (_, _, x) = x

howManyFall :: (PointSet, BrickMap, [Brick]) -> [Int]
howManyFall (ps, bm, bs) = map (\b -> chainReaction (disintegrate ps b) (bs\\[b])) bs

solve :: [Brick] -> Int
solve = countNotSupporting . fillPointSet . simulateDrops

solve2 :: [Brick] -> Int
solve2 = foldl (+) 0 . howManyFall . fillPointSet . simulateDrops

main :: IO ()
main = interact ((++"\n") . show . solve2 . parse)

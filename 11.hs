import Aoc
import qualified Data.Vector as V

type Pos = (Int, Int)

parse :: String -> [Pos]
parse = concat . zipWith remap [0..] . lines
  where
    remap row line = map (g row) . filter (f row) $ zip [0..] line
    g row (col,  _) = (row, col)
    f row (col, '#') = True
    f  _      _      = False

pSums :: Int -> [Int] -> V.Vector Int
pSums w ps = V.generate (maxPos + 2) f
  where 
    maxPos = last ps
    f i = (w - 1) * (i - (length . filter (<i)) ps)

singleDist :: [Pos] -> V.Vector Int -> V.Vector Int -> Int
singleDist (p@(x, y):ps) rs cs = sum . map dist $ ps
  where
    dist (x', y') = sum . map abs $ [x-x', y-y', (rs V.! x) - (rs V.! x'), (cs V.! y) - (cs V.! y')]

computeDist :: Int -> [Pos] -> Int
computeDist w ps = accDist 0 ps
  where
    rowSums = pSums w . nub . sort . map fst $ ps
    colSums = pSums w . nub . sort . map snd $ ps
    accDist sm    []   = sm
    accDist sm  (p:ps) = accDist ((singleDist (p:ps) rowSums colSums) + sm) ps

solve :: [Pos] -> Int
solve = computeDist 2

solve2 :: [Pos] -> Int
solve2 = computeDist 1000000

main :: IO ()
main = interact (show . solve2 . parse)

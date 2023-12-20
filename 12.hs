import Aoc
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Function.Memoize
import Debug.Trace

type Rx = (String, [Int])

parse :: String -> [Rx]
parse = map lineToRx . lines
  where
    lineToRx l = (head splits, (map read . splitOn "," . last) splits)
      where
        splits = splitOn " " l

noDots :: Int -> String -> Bool
noDots i = (all (/='.')) . (take i)

noHashs :: String -> Bool
noHashs = all (/='#')

noHashAtPos :: Int -> String -> Bool
noHashAtPos i = noHashs . take 1 . drop i

countRecursive :: Rx -> Int
countRecursive = memoize countCorrect

countCorrect :: Rx -> Int
countCorrect (s,         [])      = fromEnum $ noHashs s
countCorrect (s,         xs)
  | sum xs > length s || s == ""  = 0
countCorrect (('#':s), (x:xs))    = if noDots (x-1) s && noHashAtPos (x-1) s then countRecursive (drop (x) s, xs) else 0
countCorrect (('.':s),   xs)      =  countRecursive (s, xs)
countCorrect (('?':s),   xs)      = (countRecursive (s, xs)) + (countRecursive (('#':s), xs))

debug :: (Int, Rx) -> Int
debug (i, rx)
 | trace (show i) False = undefined
 | otherwise = countRecursive rx

solve :: [Rx] -> Int
solve = sum . map debug . zip [0,1..]

solve2 :: [Rx] -> Int
solve2 = solve . map mapRx
  where 
    mapRx (s, seqs) = (intercalate "?" (replicate 5 s), concat (replicate 5 seqs))

main :: IO ()
main = interact ((++"\n") . show . solve2 . parse)
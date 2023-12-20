import Aoc
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Function.Memoize
import Debug.Trace

parseCols :: String -> [String]
parseCols = splitOnNewline 

countOs :: String -> Int
countOs   []   = 0
countOs (x:xs) = (if x == 'O' then 1 + (length xs) else 0) + (countOs xs)

rotateAround :: [String] -> [String]
rotateAround = rtaHelper 4
  where
    rtaHelper 0 c = c
    rtaHelper x c = rtaHelper (x-1) ((map reverse . transpose . moveOs) c) 

moveOs :: [String] -> [String]
moveOs = transpose . (map (fHelper 0 0)) . transpose
  where
    fHelper ds os    []    = rep ds os
    fHelper ds os ('#':xs) = (rep ds os) ++ "#" ++ (fHelper 0 0 xs)
    fHelper ds os ('.':xs) = fHelper (ds+1) os   xs
    fHelper ds os ('O':xs) = fHelper   ds (os+1) xs
    rep ds os = (replicate os 'O') ++ (replicate ds '.')

--- 999
--- 632
--- 0 

rotateTimes :: Int -> M.Map [String] Int -> [String] -> [String]
rotateTimes 0 _ cols = cols
rotateTimes x m cols
  | M.member cols m = rotateTimes ((x `mod` (df)) - 1) m (rotateAround cols)
  | otherwise       = rotateTimes (x-1) (M.insert cols x m) (rotateAround cols)
    where 
      df = (m M.! cols) - x

      
solve :: [String] -> Int
solve = sum . map countOs . transpose . moveOs

--solve2 :: [String] -> Int
solve2 =  sum . map countOs . transpose . rotateTimes 1000000000 M.empty

prnt [] = ""
prnt (x:xs) = x ++ "\n" ++ (prnt xs)

main :: IO ()
main = interact ((++"\n") . show . solve2 . parseCols)
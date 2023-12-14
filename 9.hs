import Aoc
import Data.List.Split
import Data.Char
import Data.List
import Data.Maybe


predict :: [Int] -> Int
predict row
  | all (==0) row = 0
  | otherwise     = (last row) + (predict (differences row))
  where
    differences s = zipWith (-) (tail s) s

parseInput :: String -> [[Int]]
parseInput s = map toInt inputRows
  where
    toInt row = map (read :: String -> Int) (words row)
    inputRows = lines s

solve :: [[Int]] -> Int
solve rows = foldl (+) 0 $ map predict rows

solve2 rows = solve (map reverse rows)

main = do
  contents <- getContents
  print $ solve (parseInput contents)
  print $ solve2 (parseInput contents)
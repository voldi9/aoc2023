import Aoc
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Function.Memoize
import Debug.Trace

parse :: String -> [String]
parse = splitOnBlankLine

-- debug :: (Int, Rx) -> Int
-- debug (i, rx)
--  | trace (show i) False = undefined
--  | otherwise = countRecursive rx


flipAllRows :: [[Int]] -> [[[Int]]]
flipAllRows sq = concatHelper [] (head sq) (tail sq)
  where
    variationsForRow row = multiHelper [] (head row) (tail row)
    multiHelper p e []   = [(p ++ [1-e])] 
    multiHelper p e s    = (p ++ ((1-e):s)) : (multiHelper (p++[e]) (head s) (tail s))
    concatHelper p e []  = map (\x -> p ++ [x]) (variationsForRow e)
    concatHelper p e s   = (map (\x -> p ++ [x] ++ s) (variationsForRow e)) ++ concatHelper (p++[e]) (head s) (tail s)

convert :: Char -> Int
convert c = if c == '#' then 1 else 0

strToIntFlip :: String -> [[[Int]]]
strToIntFlip st = flipAllRows st' -- flipAllRows st'  --strToIntHelper [] (head st') (tail st')
  where
    st' = map (map convert) (splitOnNewline st)


strToInt :: String -> [[Int]]
strToInt s = map (map convert) $ splitOnNewline s

hash :: Int -> [Int] -> Int
hash sm   []   = sm
hash sm (x:xs) = hash (2*sm + x) xs

transform :: [[Int]] -> ([Int], [Int])
transform s = (map (hash 0) rows,  map (hash 0) (transpose rows))
  where 
    rows = s

cutFirstLast :: [a] -> [a]
cutFirstLast = init . tail

isPalindrome :: [Int] -> Bool
-- isPalindrome xs
--   | trace (show xs) False = undefined
isPalindrome [] = True
isPalindrome xs = head xs == last xs && isPalindrome (cutFirstLast xs)

findPalindrome :: [Int] -> Int
findPalindrome []     = 0
findPalindrome (x:[]) = 0
findPalindrome xs
  | (odd . length) xs = findPalindrome (tail xs)
  | isPalindrome   xs = (length xs) `div` 2
  | otherwise         = findPalindrome (tail . tail $ xs)

maxPalindrome :: ([Int], [Int]) -> Int
maxPalindrome (rows, cols) = max (100 * (mph rows)) (mph cols) -- max maxPalindromeRows (maxPalindromeCols)
  where
    mph sq = max (if pal == 0 then 0 else (length sq) - pal) revpal
      where
        pal = findPalindrome sq
        revpal = findPalindrome (reverse sq)

solve :: [String] -> Int
solve = sum . map maxPalindrome . map transform .  map strToInt

solve2single :: String -> Int
solve2single s = (maximum . filter (/=ogMax) . map maxPalindrome . map transform . strToIntFlip) s
  where
    ogMax = (maxPalindrome . transform . strToInt) s

solve2 :: [String] -> Int
solve2 = sum . map solve2single

main :: IO ()
main = interact ((++"\n") . show . solve2 . parse)
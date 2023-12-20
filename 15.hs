import Aoc
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import Data.Function.Memoize
import Debug.Trace
import Data.Char (ord)

type Lens = (String, Int)
type LensList = [[Lens]]

parseLenses :: String -> [String]
parseLenses = splitOn "," . concat . splitOnNewline 

hash :: String -> Int
hash = hHelper . reverse
  where
    hHelper  []   = 0
    hHelper (c:xs) = (17 * ((fromEnum c) + (hHelper xs))) `mod` 256
      
updateLists :: LensList -> String -> LensList
updateLists lists lens
  | last lens == '-' = doOnList lists ((init lens), 0) removeLens
  | otherwise        = doOnList lists (label, (read :: String -> Int) focus) addLens
    where [label, focus] = splitOn "=" lens

doOnList :: LensList -> Lens -> (Lens -> [Lens] -> [Lens]) -> LensList
doOnList lists lens@(label, _) fn = prefix ++ [fn lens list] ++ suffix
  where
    listIndex = hash label
    prefix = take listIndex lists
    (list:suffix) = drop listIndex lists

removeLens :: Lens -> [Lens] -> [Lens]
removeLens = deleteBy (\l1 l2 -> fst l1 == fst l2)

addLens :: Lens -> [Lens] -> [Lens]
addLens = addLensHelper []
  where
    addLensHelper ps l' [] = ps ++ [l']
    addLensHelper ps l' (l:ls)
      | fst l' == fst l = ps ++ [l'] ++ ls
      | otherwise       = addLensHelper (ps++[l]) l' ls

perBox :: [Lens] -> Int
perBox [] = 0
perBox (x:xs) = (noBox + 1) * slot * (snd x) + (perBox xs)
  where
    noBox = hash (fst x)
    slot = 1 + length xs

solve :: [String] -> Int
solve = sum . map hash

solve2 :: [String] -> Int
solve2 = sum . map (perBox . reverse) . foldl updateLists (replicate 256 [])

main :: IO ()
main = interact ((++"\n") . show . solve2 . parseLenses)
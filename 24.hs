import Debug.Trace
import Data.List (nub, sort, (\\))
import Data.List.Split
import qualified Data.Map as Map
import qualified Data.Set as Set

type Pos = (Int, Int, Int)
data Hail = Hail { pos, move :: Pos }
  deriving (Show, Read, Eq, Ord)

constructHail :: String -> Hail
constructHail line = Hail (strToInts position) (strToInts movement)
  where 
    [position, movement] = splitOn "@" $ filter (/=' ') line
    strToInts s = (x,y,z) where [x,y,z] = map read (splitOn "," s)

parse :: String -> [Hail]
parse = map constructHail . lines

withinBounds :: (Float, Float) -> Bool
withinBounds (x, y) = x >= minC && y >= minC && x <= maxC && y <= maxC
  where (minC, maxC) = (200000000000000, 400000000000000)

float :: (Integral a, Num b) => a -> b
float = fromIntegral

(//) :: Int -> Int -> Float
a // b = (float a) / (float b)

almostEqual :: Float -> Int -> Bool
almostEqual f i = abs ((float i) - f) < 0.000001

isNotInPast :: Hail -> (Float, Float) -> Bool
isNotInPast (Hail (x, y, z) (dx, dy, dz)) (x', y') = almostEqual x' x || signum (x' - (float x)) == signum (float dx)

isCrossing :: Hail -> Hail -> Bool
isCrossing h1@(Hail (x1, y1, z1) (dx1, dy1, dz1)) h2@(Hail (x2, y2, z2) (dx2, dy2, dz2))
  | dx2 * dy1 == dx1 * dy2 = (x1, y1) == (x2, y2)
  | otherwise = withinBounds (x,y) && isNotInPast h1 (x,y) && isNotInPast h2 (x,y)
    where
      m1 = dy1//dx1
      b1 = float y1 - m1*(float x1)
      m2 = dy2//dx2
      b2 = float y2 - m2*(float x2)
      x = (b2 - b1) / (m1 - m2) 
      y = m1 * x + b1
  
countCrossing :: [Hail] -> Int
countCrossing   []   = 0
countCrossing (h:hs) = countCrossing hs + (length (filter (isCrossing h) hs))

main :: IO ()
main = interact ((++"\n") . show . countCrossing . parse)

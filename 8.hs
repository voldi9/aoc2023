import Aoc
import Data.List.Split
import Data.Char
import Data.List
import Data.Maybe

import Data.Map (Map, fromList, elems, (!), keys)

data Node = Node String Node Node
  deriving Eq

nodeName :: Node -> String 
nodeName (Node n _ _) = n

endingNode :: Node -> Bool
endingNode n = last (nodeName n) == 'Z'

leftChild :: Node -> Node 
leftChild (Node _ l _) = l

rightChild :: Node -> Node 
rightChild (Node _ _ r) = r

parseNode :: String -> (String, (String, String))
parseNode s = (name s, (left s, right s))
  where
    name = take 3
    left = name . (drop 3)
    right = left . (drop 3)

createNode :: Map String (String, String) -> String -> Node
createNode mp label = Node label (createNode mp (fst mpVal)) (createNode mp (snd mpVal))
  where
    mpVal = mp ! label

nodeMap :: [String] -> Map String (String, String)
nodeMap s = fromList $ map (parseNode . (filter isLetter)) s

buildGraph :: (String -> Bool) -> Map String (String, String) -> [Node]
buildGraph f mp = map (\x -> createNode mp x) $ filter f (keys mp)

cycleForNode :: String -> Node -> Int
cycleForNode = cfn 0
  where
    cfn c (x:xs) n
      | endingNode n = c
      | x == 'L'     = cfn (c+1) xs (leftChild n)
      | x == 'R'     = cfn (c+1) xs (rightChild n)

multiTraverseGraph :: String -> [Node] -> [Int]
multiTraverseGraph s n = map (cycleForNode s) n

parseInput :: (String -> Bool) -> String -> (String, [Node])
parseInput f s = (cycle path, buildGraph f (nodeMap nodes))
  where
    [path, nodeString] = splitOnBlankLine s
    nodes = lines nodeString

solve :: (String, [Node]) -> Int
solve (path, nodes) = foldl lcm 1 $ multiTraverseGraph path nodes

main = do
  contents <- getContents
  print $ solve (parseInput (\x -> x == "AAA") contents)
  print $ solve (parseInput (\x -> x !! 2 == 'A') contents)
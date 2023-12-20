import Debug.Trace
import Data.List.Split
import qualified Data.Map as Map

type WorkflowCheck = ((Part -> Bool), Char)
data ValueRanges = ValueRanges { xr, mr, ar, sr :: [Int] }
type WorkflowStep = (WorkflowCheck, WorkflowAction)
type WorkflowMap = Map.Map String [WorkflowStep]
data Part = Part { x, m, a, s :: Int }
  deriving Show
data WorkflowAction = Accept | Reject | Goto String
  deriving Show

initRange :: ValueRanges
initRange = ValueRanges [1..4000] [1..4000] [1..4000] [1..4000] 

parseLabel :: String -> WorkflowAction
parseLabel lbl
  | lbl == "A" = Accept
  | lbl == "R" = Reject
  | otherwise  = Goto lbl

parseCheck :: String -> WorkflowCheck
parseCheck chk = case take 2 chk of
                "x>" -> (((>val) . x), 'x')
                "x<" -> (((<val) . x), 'x')
                "m>" -> (((>val) . m), 'm')
                "m<" -> (((<val) . m), 'm')
                "a>" -> (((>val) . a), 'a')
                "a<" -> (((<val) . a), 'a')
                "s>" -> (((>val) . s), 's')
                "s<" -> (((<val) . s), 's')
  where 
    val = read $ drop 2 chk

parseWorkflowStep :: String -> WorkflowStep
parseWorkflowStep s = (check, action)
  where
    splits = splitOn ":" s
    (check, action) = case length splits of
                        1 -> ((\_ -> True, 'x'), parseLabel s)
                        2 -> (parseCheck (head splits), parseLabel (last splits))

parseWorkflow :: String -> (String, [WorkflowStep])
parseWorkflow w = (name, (map parseWorkflowStep actions))
  where
    [name, rest] = splitOn "{" (init w)
    actions = splitOn "," rest

sumPart :: Part -> Int
sumPart (Part x m a s) = x + m + a + s

printRange :: [Int] -> String
printRange [] = "[]"
printRange [i] = "[" ++ show i ++ "]"
printRange l = "[" ++ show (head l) ++ ".." ++ show (last l) ++ "]"

multiplyRanges :: ValueRanges -> Integer
multiplyRanges (ValueRanges x m a s)
  | trace ("Accepted: x=" ++ printRange x ++ ", m=" ++ printRange m ++ ", a=" ++ printRange a ++ ", s=" ++ printRange s) False = undefined
multiplyRanges (ValueRanges x m a s) = (fromIntegral (length x)) 
                                    * (fromIntegral (length m)) 
                                    * (fromIntegral (length a)) 
                                    * (fromIntegral (length s))

evaluatePartHelper :: WorkflowMap -> [WorkflowStep] -> Part -> Int
evaluatePartHelper wm (step:steps) part
  | (fst (fst step)) part == False = evaluatePartHelper wm steps part
  | otherwise = case snd step of
                 Reject -> 0
                 Accept -> sumPart part
                 Goto newWorkflow -> evaluatePartHelper wm (wm Map.! newWorkflow) part

evaluatePart :: WorkflowMap -> Part -> Int
evaluatePart wm part = evaluatePartHelper wm (wm Map.! "in") part

rangesToParts :: Char -> ValueRanges -> [Part]
rangesToParts 'x' vr = [Part x 0 0 0 | x <- xr vr]
rangesToParts 'm' vr = [Part 0 m 0 0 | m <- mr vr]
rangesToParts 'a' vr = [Part 0 0 a 0 | a <- ar vr]
rangesToParts 's' vr = [Part 0 0 0 s | s <- sr vr]

filterRanges :: ValueRanges -> WorkflowCheck -> Bool -> ValueRanges
filterRanges vr@(ValueRanges xr mr ar sr) chk b = case snd chk of
  'x' -> (ValueRanges (map x filteredParts) mr ar sr)
  'm' -> (ValueRanges xr (map m filteredParts) ar sr)
  'a' -> (ValueRanges xr mr (map a filteredParts) sr)
  's' -> (ValueRanges xr mr ar (map s filteredParts))
  where
    filteredParts = (filter ((==b) . (fst chk)) . rangesToParts (snd chk)) vr

evaluateAllParts :: WorkflowMap -> [WorkflowStep] -> ValueRanges -> Integer
evaluateAllParts wm      []      ranges = 0 
evaluateAllParts wm (step:steps) ranges 
  | trace ("evaluateAllParts snd step = " ++ show (snd step) ++ ".") False = undefined
evaluateAllParts wm (step:steps) ranges = 
    failedCheckEval + case snd step of
      Accept   -> multiplyRanges passedCheckRange
      Reject   -> 0
      Goto lbl -> evaluateAllParts wm (wm Map.! lbl) passedCheckRange
  where
    passedCheckRange = filterRanges ranges (fst step) True 
    failedCheckRange = filterRanges ranges (fst step) False
    failedCheckEval  = evaluateAllParts wm steps failedCheckRange

parsePart :: String -> Part
parsePart s = Part vx vm va vs
  where
    vals = splitOn "," (tail (init s))
    extractVal = read . drop 2
    [vx, vm, va, vs] = map extractVal vals

parse :: String -> (WorkflowMap, [Part])
parse s = (parsedWorkflows, parsedParts)
  where
    [workflowsString, partsString] = splitOn "\n\n" s
    parsedWorkflows = Map.fromList $ map parseWorkflow (lines workflowsString)
    parsedParts = map parsePart (lines partsString)

solve1 :: String -> Int
solve1 s = sum $ map (evaluatePart workflows) parts
  where
    (workflows, parts) = parse s

solve2 :: String -> Integer
solve2 s = evaluateAllParts workflows (workflows Map.! "in") initRange
  where
    workflows = fst (parse s)

main :: IO ()
main = interact ((++"\n") . show . solve2)

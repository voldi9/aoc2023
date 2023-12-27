import Prelude hiding (flip)
import Debug.Trace
import Data.List.Split
import qualified Data.Map as Map

type ModuleMap = Map.Map String Module
type LastPulseMap = Map.Map String Pulse
type InputMap = Map.Map String [String]
type SentLowMap = Map.Map String Int
data Input = Input { pulse :: Pulse, mName :: String } 
  deriving (Show, Read, Eq, Ord)
data Pulse = High | Low
  deriving (Show, Read, Eq, Ord)
data Module = FFModule { state :: Pulse, output :: [String] } 
            | ConModule { output :: [String], sentHigh :: Bool } 
            | BroadModule { output :: [String] }
            | Out { lows :: Int }
  deriving (Show, Read, Eq, Ord)
data System = System { modules :: ModuleMap, 
                    lastPulses :: LastPulseMap,
                      inputMap :: InputMap,
                    inputQueue :: [Input],
                        pulses :: (Int, Int) }
  deriving (Show, Read, Eq, Ord)

initSystem :: System
initSystem = resetQueue $ System Map.empty Map.empty Map.empty [] (0,0)

removeQueueHead :: System -> System
removeQueueHead sys = sys { inputQueue = init (inputQueue sys) }

resetQueue :: System -> System
resetQueue sys = sys { inputQueue = [Input Low "roadcaster"] }

incrementPulses :: Pulse -> System -> System
incrementPulses Low  sys = sys { pulses = (x+1, y) }
  where (x, y) = pulses sys
incrementPulses High sys = sys { pulses = (x, y+1) }
  where (x, y) = pulses sys

flip :: Pulse -> Pulse
flip High = Low
flip Low = High

createModule :: Char -> [String] -> Module
createModule t output = case t of
  'b' -> BroadModule output
  '%' -> FFModule High output
  '&' -> ConModule output False

parseModule :: System -> String -> System
parseModule (System mm lpm im q ps) s = System mm' lpm' im' q ps
  where
    [(t:name), outputString] = splitOn " -> " s
    output = splitOn ", " outputString
    mm' = Map.insert name (createModule t output) mm
    lpm' = Map.insert name Low lpm
    im' = foldl (updateInputs name) im output
      where
        updateInputs :: String -> InputMap -> String -> InputMap
        updateInputs outName inputMap inName
          | Map.member inName inputMap = Map.adjust (++[outName]) inName inputMap
          | otherwise                  = Map.insert inName [outName] inputMap

parse :: String -> System
parse = foldl parseModule initSystem . lines

modUpdateSend :: Pulse -> Module -> Module
modUpdateSend p cm@(ConModule _ sl) = cm { sentHigh = sl || p == High }
modUpdateSend _ bm@(BroadModule _) = bm
modUpdateSend _ fm@(FFModule s o) = FFModule (flip s) o

sysUpdateSend :: System -> String -> Pulse -> [String] -> System
sysUpdateSend (System modules lastPulses imap queue pulses) name pulse output = 
  (System modules' lastPulses' imap queue' pulses)
    where
      modules' = Map.adjust (modUpdateSend pulse) name modules
      lastPulses' = Map.adjust (const pulse) name lastPulses 
      queue' = (reverse [Input pulse name | name <- output]) ++ queue

sendPulse :: System -> Module -> String -> Pulse -> System
sendPulse sys modul name pulse = sysUpdateSend sys name pulse (output modul)

processPulseForModule :: System -> Module -> String -> Pulse -> System
processPulseForModule sys (Out x) name Low = sys { modules = Map.adjust (\m -> m { lows = (lows m) + 1 }) name (modules sys) }
processPulseForModule sys (Out _)  _  High = sys

processPulseForModule sys modul@(BroadModule _) name pulse = 
  sendPulse sys modul name pulse

processPulseForModule sys modul@(FFModule state _) name pulse = case pulse of
  High -> sys
  Low -> sendPulse sys modul name state

processPulseForModule sys modul@(ConModule output _) name pulse
  | all (==High) . map ((lastPulses sys) Map.!) . ((inputMap sys) Map.!) $ name = sendPulse sys modul name Low
  | otherwise = sendPulse sys modul name High

processPulse :: System -> Input -> System
processPulse sys (Input pulse mName) = (incrementPulses pulse . removeQueueHead) (updateSys sys mName pulse)
  where
    updateSys sys' name pulse
      | Map.notMember name (modules sys') = sys' { modules = Map.insert name (Out 0) (modules sys') }
      | otherwise = processPulseForModule sys' ((modules sys') Map.! name) name pulse

processPulses :: System -> System
processPulses sys@(System _ _ _ [] _) = sys
processPulses sys = (processPulses . processPulse sys . last . inputQueue) sys

solve :: System -> Int
solve sys = uncurry (*) . pulses $ foldl processOnce sys [1..1000]
  where
      processOnce s _ = processPulses (resetQueue s)

checkIfSentHigh :: System -> Int -> SentLowMap -> String -> SentLowMap
checkIfSentHigh sys count mp out
  | Map.member out mp = mp
  | otherwise         
    = if sentHigh ((modules sys) Map.! out) then Map.insert out count mp else mp

solve2Helper :: System -> Int -> SentLowMap -> [String] -> Int
solve2Helper sys count mp outs
  | Map.size mp == length outs = foldl lcm 1 (Map.elems mp)
  | otherwise                 
    = solve2Helper 
        (processPulses (resetQueue sys)) 
        (count+1) 
        (foldl (checkIfSentHigh sys count) mp outs) 
        outs

solve2 :: System -> Int
solve2 sys = solve2Helper sys 0 Map.empty outs
  where
    !outs = ((inputMap sys) Map.!) . head . ((inputMap sys) Map.!) $ "rx"

main :: IO ()
main = interact ((++"\n") . show . solve2 . parse)

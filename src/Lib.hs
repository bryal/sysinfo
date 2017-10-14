module Lib (prettyCpuinfo) where

import qualified Data.Map.Strict as Map
import Data.List (elemIndex, find)
import Data.List.Split (splitWhen, splitOn)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.String (fromString)
import Data.String.Utils (strip)

type Processor = Map.Map String String

parseProcessorProp :: String -> (String, String)
parseProcessorProp l = let [key, val] = (splitOn ":" l)
                       in (strip key, strip val)

parseProcessor :: String -> Processor
parseProcessor = Map.fromList . map parseProcessorProp . lines

parseProcessors :: String -> [Processor]
parseProcessors = map parseProcessor . init . splitOn "\n\n"

prettyPhysical p = let props = ["physical id", "model name", "cpu cores", "siblings"]
                       [id, model, cores, threads] = catMaybes (map (\key -> Map.lookup key p) props)
                   in "cpu " ++ id ++ ": " ++ model ++ "\n" ++
                      (replicate (length id + 6) ' ') ++ cores ++ " cores, " ++ threads ++ " threads"

takeWhileJust = catMaybes . takeWhile isJust

-- Takes the contents of "/proc/cpuinfo", extracts the intresting parts, and returns a prettier
-- and more printable string containing the most interesting parts of the "cpuinfo"
prettyCpuinfo :: String -> String
prettyCpuinfo s = let processors = (parseProcessors s)
                      physicalCpus = (uniquePhysicalCpus processors)
                      cores = show (countCores physicalCpus)
                      threads = show (countThreads physicalCpus)
                  in unlines ((map prettyPhysical physicalCpus) ++
                              ["total: " ++ cores ++ " cores, " ++ threads ++ " threads" ])
  where uniquePhysicalCpus ps = takeWhileJust (map (\id -> find (hasPhysicalId id) ps) [0..])
        hasPhysicalId id = (== Just (show id)) . (Map.lookup "physical id")
        countNums :: String -> [Processor] -> Int
        countNums key cpus = sum (map (read . fromJust . (Map.lookup key)) cpus)
        countCores = countNums "cpu cores"
        countThreads = countNums "siblings"

module Lib (prettyCpuinfo) where

import qualified Data.Map.Strict as Map
import Data.List (elemIndex, find)
import Data.List.Split (splitWhen, splitOn)
import Data.Maybe (catMaybes, fromJust, isJust)
import Data.String (fromString)
import Data.String.Utils (strip)

type Processor = Map.Map String String

parseProcessorProp :: String -> Maybe (String, String)
parseProcessorProp l = let r = (splitOn ": " l)
                       in if (length r) == 2
                          then Just (strip (r !! 0), strip (r !! 1))
                          else Nothing

parseProcessor :: String -> Processor
parseProcessor s = Map.fromList (catMaybes (map parseProcessorProp (lines s)))

parseProcessors :: String -> [Processor]
parseProcessors s = map parseProcessor (init (splitOn "\n\n" s))

prettyPhysical p = let props = ["physical id", "model name", "cpu cores", "siblings"]
                       vals = catMaybes (map (\key -> Map.lookup key p) props)
                       id = vals !! 0
                       (cores, threads) = ((vals !! 2), (vals !! 3))
                   in "cpu " ++ id ++ ": " ++ (vals !! 1) ++ "\n" ++
                      (replicate (length id + 6) ' ') ++ cores ++ " cores, " ++ threads ++ " threads"

-- Takes the contents of "/proc/cpuinfo", extracts the intresting parts, and returns a prettier
-- and more printable string containing the most interesting parts of the "cpuinfo"
prettyCpuinfo :: String -> String
prettyCpuinfo s = let processors = (parseProcessors s)
                      physicalCpus = (uniquePhysicalCpus processors)
                      cores = show (countCores physicalCpus)
                      threads = show (countThreads physicalCpus)
                  in unlines ((map prettyPhysical physicalCpus) ++
                              ["total: " ++ cores ++ " cores, " ++ threads ++ " threads" ])
  where uniquePhysicalCpus ps =
          catMaybes (takeWhile isJust
                               (map (\id -> find ((== Just (show id)) . (Map.lookup "physical id"))
                                            ps)
                                    [0..]))
        countNums :: String -> [Processor] -> Int
        countNums key cpus = sum (map (read . fromJust . (Map.lookup key)) cpus)
        countCores = countNums "cpu cores"
        countThreads = countNums "siblings"

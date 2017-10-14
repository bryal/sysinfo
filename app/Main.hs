module Main where

import Lib

main = do cpuinfo <- readFile "/proc/cpuinfo"
          putStr (prettyCpuinfo cpuinfo)


module AoC2020.Day7 where

import Data.List.Split(splitOn)
import Data.List

solveBoth :: IO ()
solveBoth = do
    allGroups <- splitOn "\n\n" <$> readFile "./infiles/AoC2020/Day7.in"




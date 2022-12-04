module AoC2022.Day1 where

import Text.Read (readMaybe)
import Data.List.Split(splitOn)
import Data.Maybe (mapMaybe)
import Data.List (sort)

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day1.in"
    print $ mostCalories $ parse fileContents
    print $ topThreeCalories $ parse fileContents

parse :: String -> [[Int]]
parse = mapMaybe (traverse readMaybe . lines) . splitOn "\n\n"

mostCalories :: [[Int]] -> Int
mostCalories = maximum . fmap sum

topThreeCalories :: [[Int]] -> Int
topThreeCalories = sum . take 3 . reverse . sort . fmap sum 

module AoC2022.Day6 where

import Data.Maybe (maybe)
import Data.List (findIndex, nub, subsequences)

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day6.in"
    print $ findNDistinct 4 fileContents
    print $ findNDistinct 14 fileContents

findNDistinct :: Int -> String -> Int
findNDistinct n = maybe 0 (+n) . findIndex ((==n) . length . nub) . asNGroups n

asNGroups :: Int -> String -> [String]
asNGroups n [] = []
asNGroups n xs = take n xs : asNGroups n (drop 1 xs)

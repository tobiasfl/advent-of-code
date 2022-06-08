
module AoC2020.Day6 where

import Data.List.Split(splitOn)
import Data.List

solveBoth :: IO ()
solveBoth = do
    allGroups <- splitOn "\n\n" <$> readFile "./infiles/AoC2020/Day6.in"
    print $ solveA allGroups
    print $ solveB allGroups

solveA :: [String] -> Int
solveA = sum . map (length . nub . filter (/='\n'))

--TODO: not finished
solveB :: [String] -> Int
solveB = sum . map (length . nub . foldr intersect ['a'..'z'] . splitOn "\n")

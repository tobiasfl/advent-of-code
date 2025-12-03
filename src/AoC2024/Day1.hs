module AoC2024.Day1 where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List (sort, transpose)
import Debug.Trace (trace)

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2024/Day1.in"
    let lists = parse fileContents
    print $ totalDistance lists
    print $ totalSimilarity lists
    

parse :: String -> [[Int]]
parse = transpose . mapMaybe (traverse readMaybe . words) . lines

totalDistance :: [[Int]] -> Maybe Int
totalDistance [y, x] = Just $ sum $ zipWith diff (sort y) (sort x)
totalDistance _ = Nothing

diff :: Int -> Int -> Int
diff l r = abs $ l - r 

totalSimilarity :: [[Int]] -> Maybe Int
totalSimilarity [x, y] = Just $ sum $ map (\lnum -> lnum * occurrences lnum y) x
totalSimilarity _ = Nothing

occurrences :: Int -> [Int] -> Int
occurrences x = length . filter (x==)

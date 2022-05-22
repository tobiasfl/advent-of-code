module AoC2021.Day6 where

import System.Environment   
import Data.List.Split(splitOn)
import qualified Data.Map as Map

solveBoth :: IO ()
solveBoth = do
    contents <- readFile "./infiles/Day6.in"
    let counterMap = toCounterMap $ map read $ splitOn "," contents
    print $ countFish 80 counterMap
    print $ countFish 256 counterMap

toCounterMap :: [Int] -> Map.Map Int Int
toCounterMap = foldr (Map.adjust (1+)) (Map.fromList [(k, 0) | k <- [0..8]]) 

countFish :: Int -> Map.Map Int Int -> Int
countFish 0 = sum . Map.elems 
countFish n = countFish (n-1) . updateCounters 

updateCounters :: Map.Map Int Int -> Map.Map Int Int
updateCounters = updateNew . Map.mapAccumRWithKey updateRest 0
    where updateRest acc k v = (v, acc)
          updateNew (a, m) = Map.adjust (a+) 6 $ Map.adjust (a+) 8 m 


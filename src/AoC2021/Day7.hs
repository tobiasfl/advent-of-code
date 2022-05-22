module AoC2021.Day7 where

import System.Environment   
import Data.List
import Data.List.Split(splitOn)

solveBoth :: IO ()
solveBoth = do
    (contents:xs) <- lines <$> readFile "./infiles/Day7.in"
    let nums = map read $ splitOn "," contents
    print $ constCostAtPos (median nums) nums
    let withFloor = linearCostAtPos (floor $ avg nums) nums 
    let withCeil = linearCostAtPos (ceiling $ avg nums) nums 
    print $ min withFloor withCeil



constCostAtPos :: Int -> [Int] -> Int
constCostAtPos p = sum . map (diff p)

diff :: Int -> Int -> Int
diff x y = max x y - min x y

median :: [Int] -> Int
median xs = sort xs !! (length xs `div` 2)

linearCostAtPos :: Int -> [Int] -> Int
linearCostAtPos p = sum . map (linearCostBetween p)

linearCostBetween :: Int -> Int -> Int
linearCostBetween a b = sum [0..diff a b]

avg :: [Int] -> Double
avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

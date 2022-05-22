module AoC2021.Day1 where

import System.Environment   
import Data.List  

solveA :: String -> Int
solveA = countIncreases . map read . words

solveB :: String -> Int
solveB = countIncreases . toSums . map read . words

countIncreases :: [Int] -> Int
countIncreases [] = 0
countIncreases xs =  sum $ zipWith (\x y -> if y > x then 1 else 0) xs (tail xs)

toSums :: [Int] -> [Int]
toSums xs = map (\(x,y,z) -> x+y+z) $ zip3 xs (tail xs) (tail $ tail xs)

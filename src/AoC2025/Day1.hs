module AoC2025.Day1 where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List
import Debug.Trace (trace)

example :: [Int]
example = [-68, -30, 48, -5, 60, -55, -1, -99, 14, -82]

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2025/Day1.in"
    let instructions = parse fileContents
    print $ numZeros instructions
    print $ allZeroPassings instructions


parse :: String -> [Int]
parse =  mapMaybe parseInt . lines
    where parseInt ('R':xs) = readMaybe xs
          parseInt ('L':xs) = negate <$> readMaybe xs
          parseInt _ = Nothing

numZeros :: [Int] -> Int
numZeros = length . filter (0==) . snd . mapAccumL (\dial instr -> (turn dial instr, dial)) 50

turn :: Int -> Int -> Int
turn dial instr
   | dialRaw < 0 = (100 + dialRaw) `mod` 100
   | otherwise = dialRaw `mod` 100
   where dialRaw =  dial + instr

allZeroPassings :: [Int] -> Int
allZeroPassings instructions = length $ filter (0==) $ allDialStops instructions 50

allDialStops :: [Int] -> Int -> [Int]
allDialStops (instr:xs) dial
  | instr > 0 = dial : allDialStops (instr - 1:xs) (turn dial 1)
  | instr < 0 = dial : allDialStops (instr + 1:xs) (turn dial (- 1))
  | otherwise = allDialStops xs dial
allDialStops [] dial = [dial]

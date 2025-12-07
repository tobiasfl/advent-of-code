
module AoC2025.Day3 where

import Text.Read (readMaybe)
import Data.List
import Data.List.Split
import Data.List.Extra
import Data.Maybe
import Debug.Trace (trace)

example :: [String]
example = [ "987654321111111", "811111111111119", "234234234234278", "818181911112111" ]

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2025/Day3.in"
    let batteryBanks = lines fileContents
    print $ totalOutputJoltage batteryBanks

totalOutputJoltage :: [String] -> Int
totalOutputJoltage = sum . map maxBankJoltage

maxBankJoltage :: String -> Int
maxBankJoltage xs = fromJust $ find (\n -> findNum (show n) xs == show n) [99, 98..10]

findNum :: String -> String -> String
findNum n xs = take 1 lastPart ++ take 1 (dropUntilChar (last n) (drop 1 lastPart))
    where lastPart = dropUntilChar (head n) xs

dropUntilChar :: Char -> String -> String
dropUntilChar n = dropWhile (n/=)

--maxBankJoltage12 :: String -> Int
--maxBankJoltage12 xs = take 12 $ iterate (\(acc, rest) -> (acc ++ [head (findNextHighest rest)], findNextHighest rest)) ([], xs)
--
--findNextHighest :: String -> String
--findNextHighest xs = take 1 $ fmap  ['9', '8'..'0']

--maxBankJoltage = maximum . map read . filter ((==2) . length) . subsequences

--look for 9, if found, search the rest for 9, 8, ...

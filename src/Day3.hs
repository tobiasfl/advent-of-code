module Day3 where

import System.Environment   
import Data.List  
import Data.Char(digitToInt)
import Data.Bifunctor(first)
import Lib(binToInt)

solveBoth :: IO ()
solveBoth = do
    (filename:args) <- getArgs
    contents <- readFile filename 
    print $ solveA $ lines contents 
    print $ solveB $ lines contents

example :: [String]
example = [
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010" ]

solveA :: [String] -> Int
solveA xs = (\ys -> binToInt ys * binToInt (revBin ys)) . map dominantBit $ transpose xs

revBin :: String -> String
revBin = map (\b -> if b == '1' then '0' else '1')

dominantBit :: String -> Char
dominantBit xs =  if sum (map digitToInt xs) >= ((length xs+1) `div` 2) then '1' else '0'

solveB :: [String] -> Int
solveB xs = binToInt (findCo2 (xs, xs)) * binToInt (findOxygen (xs, xs))

findOxygen :: ([String], [String]) -> String
findOxygen (_, []) = ""
findOxygen (_, [x]) = x
findOxygen (trunced, xs) = findOxygen $ search (dominantBit (head $ transpose trunced)) (trunced, xs)


findCo2 :: ([String], [String]) -> String
findCo2 (_, []) = ""
findCo2 (_, [x]) = x
findCo2 (trunced, xs) = findCo2 $ search (head $ revBin [dominantBit (head $ transpose trunced)]) (trunced, xs)


--gets one to use for filtering, one to filter out of, 
--produces one version of filtered with first bit removed and one original
search :: Char -> ([String], [String]) -> ([String], [String])
search b (trunced, xs) = unzip $ map (first tail) $ filter ((b==) . head . fst) both
    where both = zip trunced xs

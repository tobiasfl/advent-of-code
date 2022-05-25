module AoC2020.Day2 where

import System.Environment   
import Text.Parsec as Parsec
import Text.Parsec ((<|>))
import Data.Either

type InpLine = (Int, Int, Char, String)

solveBoth :: IO ()
solveBoth = do
    allInp <- rights . map (parse parseInpLine "") . lines <$> readFile "./infiles/AoC2020/Day2.in"
    print $ solveA allInp 
    print $ solveB allInp 

solveA :: [InpLine] -> Int
solveA [] = 0
solveA ((min, max, c, p):xs) = (if count >= min && count <= max then 1 else 0) + solveA xs
    where count = length $ filter (c ==) p

solveB :: [InpLine] -> Int
solveB [] = 0
solveB ((i1, i2, c, p):xs) = (if valid then 1 else 0) + solveB xs
    where valid = length (filter ((c ==) . (p !!)) [i1-1, i2-1]) == 1

parseInpLine :: Parsec.Parsec String () InpLine
parseInpLine = do
    minLim <- Parsec.many1 Parsec.digit
    Parsec.char '-'
    maxLim <- Parsec.many1 Parsec.digit
    Parsec.space
    c <- Parsec.oneOf ['a'..'z']
    Parsec.skipMany1 (Parsec.oneOf ": ")
    password <- Parsec.many1 (Parsec.oneOf ['a'..'z'])
    return (read minLim, read maxLim, c, password)
    



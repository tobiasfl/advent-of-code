module AoC2021.Day14 where

import Data.List.Split(splitOn)
import System.Environment   
import Data.List
import Data.Char(digitToInt, isUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Bifunctor(first)


solveBoth :: IO ()
solveBoth = do
    (temp, rules) <- parseInput . lines <$> readFile "./infiles/Day14.in"
    print $ solveA (temp, rules) 10
    print $ solveA (temp, rules) 40

type Rules = Map String Char

solveA :: (String, Rules) -> Int -> Int
solveA (temp, rules) n = toResult (iterate (step rules) temp !! max 0 n)
    where toResult = (\gs -> length (last gs) - length (head gs)) . sortOn length . group . sort

--TODO: naive solution
step :: Rules -> String -> String
step rs (x:y:xs) = case Map.lookup [x,y] rs of Just z  -> x:z:step rs (y:xs)
                                               Nothing -> x:step rs (y:xs)
step _ xs = xs

parseInput :: [String] -> (String, Rules) 
parseInput (x:xs) = (x, Map.fromList $ map (toTuple . splitOn " -> ") xs)
    where toTuple (x:y:xs) = (x, head y)
          toTuple _ = ("", 'e')
parseInput _ = ("", Map.empty)

--make map (char, char) char and another map (char, char) int that keeps count of rule applications.. i think

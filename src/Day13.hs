module Day13 where

import Lib(mkGridMap, allCoords, GridMap, neighborCoords8)
import System.Environment   
import Data.List
import Data.Char(digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Stack
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import Data.List.Split(splitOn)

solveBoth :: IO ()
solveBoth = do
    (holeSet, folds) <- parseInput . lines <$> readFile "./infiles/Day13Test.in"
    print $ solveA (head folds) holeSet
    print folds
    print holeSet
    print $ solveB (init folds) holeSet
    print $ solveB folds holeSet

data Fold = X Int | Y Int deriving (Show, Eq)

parseInput :: [String] -> (Set (Int, Int), [Fold])
parseInput = (\(holeCoords:foldLines:xs) -> (parseHoles holeCoords, parseFolds foldLines)) . splitOn [""]

parseHoles :: [String] -> Set (Int, Int)
parseHoles = Set.fromList . map ((\(x:y:xs) -> (read x, read y)) . splitOn ",")

parseFolds :: [String] -> [Fold]
parseFolds = map ((\(rest:line:xs) -> 
    if last rest == 'x' then X (read line) else Y (read line)) . splitOn "=")

solveA ::  Fold -> Set (Int, Int) -> Int
solveA f = length . Set.map (foldStep f)

solveB :: [Fold] -> Set (Int, Int) -> Set (Int, Int) 
solveB (f:fs) = solveB fs . Set.map (foldStep f)
solveB _ = id

foldStep :: Fold -> (Int, Int) -> (Int, Int)
foldStep (X xLine) (x, y) = if x > xLine then ((xLine*2)-x, y) else (y, x)
foldStep (Y yLine) (x, y) = if y > yLine then (x, (yLine*2)-y) else (y, x)

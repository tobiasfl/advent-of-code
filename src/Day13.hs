module Day13 where

import System.Environment   
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List.Split(splitOn)

solveBoth :: IO ()
solveBoth = do
    (holeSet, folds) <- parseInput . lines <$> readFile "./infiles/Day13.in"
    print $ solveA (head folds) holeSet
    putStrLn $ prettyPrint $ solveB folds holeSet

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

prettyPrint :: Set (Int, Int) -> String
prettyPrint gm = map coordToChar $ sortBy (\(x, y) (x1, y1) -> compare (y, x) (y1, x1)) $ [(x, y) | x <- [0..maxX+1], y <- [0..maxY+1]]
    where maxX = fst $ maximumBy (\(x, y) (x1, y1) -> compare x x1) gm
          maxY = snd $ maximumBy (\(x, y) (x1, y1) -> compare y y1) gm
          coordToChar (x, y) 
            | Set.member (x, y) gm = '#'
            | x > maxX             = '\n'
            | otherwise            = '.'

foldStep :: Fold -> (Int, Int) -> (Int, Int)
foldStep (X xLine) (x, y) = if x > xLine then ((xLine*2)-x, y) else (x, y)
foldStep (Y yLine) (x, y) = if y > yLine then (x, (yLine*2)-y) else (x, y)

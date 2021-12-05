module Day5 where

import System.Environment   
import Data.List  
import Data.List.Split

solveBoth :: IO ()
solveBoth = do
    realContents <- lines <$> readFile "./infiles/Day5.in"
    --realContents <- lines <$> readFile "./infiles/Day5Test.in"
    let asLines = map parseLine realContents
    let noDiags = filter (not . null) $ removeDiagonals asLines
    putStrLn "Final score:"
    print $ countOverlaps (map withBetweens noDiags)
    print $ countOverlaps (map withBetweens asLines)
   
type Line = [(Int,Int)]

parseLine :: String -> Line
parseLine = concatMap (toTuples . map read . splitOn ",") . splitOn " -> "

toTuples :: [a] -> [(a, a)]
toTuples []  = []
toTuples [x] = []
toTuples (x:y:xs) = (x,y): toTuples xs

withBetweens :: Line -> Line
withBetweens []  = []
withBetweens [x] = []
--withBetweens ((x1,y1):(x2,y2):xs) = [(x,y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]  
withBetweens ((x1,y1):(x2,y2):xs) = [(x,y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]  



removeDiagonals :: [Line] -> [Line]
removeDiagonals = filter (\xs -> notDiag (map fst xs) || notDiag (map snd xs))
    where notDiag xs = head xs == last xs

countOverlaps :: [Line] -> Int
countOverlaps = length . filter ((1<) . length) . group . sort . concat


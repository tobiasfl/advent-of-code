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
withBetweens ((x1,y1):(x2,y2):xs) 
  | notDiag ((x1,y1):(x2,y2):xs) = [(x,y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]]  
  | otherwise = (x1,y1):withBetweens ((if x1 < x2 then x1+1 else x1-1, if y1 < y2 then y1+1 else y1-1):(x2,y2):xs)

notDiag :: Line -> Bool
notDiag xs = check (map fst xs) || check (map snd xs)
    where check axisCoords = head axisCoords == last axisCoords


removeDiagonals :: [Line] -> [Line]
removeDiagonals = filter notDiag

countOverlaps :: [Line] -> Int
countOverlaps = length . filter ((1<) . length) . group . sort . concat


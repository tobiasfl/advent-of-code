module Day9 where

import System.Environment   
import Data.List
import Data.Char(digitToInt)
--import Data.List.Split(splitOn)
--import Data.Map (Map)
--import qualified Data.Map as Map
--import Control.Applicative
import Data.Maybe
import Data.Tuple

solveBoth :: IO ()
solveBoth = do
    contents <- lines <$> readFile "./infiles/Day9.in"
    --contents <- lines <$> readFile "./infiles/Day9Test.in"
    print $ solveA $ map (map digitToInt) contents

solveA :: [[Int]] -> Int
solveA = sum . map (+1) . catMaybes . findLowPoints 

findLowPoints :: [[Int]] -> [Maybe Int]
findLowPoints xs = map (`checkCoords` xs) $ [(r, c) | r <- [0..length xs-1], c <- [0..length (transpose xs)-1]]

checkCoords :: (Int, Int) -> [[Int]] -> Maybe Int
checkCoords cs = (\(x, neighs) -> if x < minimum neighs then Just x else Nothing) . allNeighbors cs

allNeighbors :: (Int, Int) -> [[Int]] -> (Int, [Int])
allNeighbors (r, c) xs = ((xs !! r) !! c, catMaybes neighbors) 
    where neighbors = [right (r, c) xs, left (r, c) xs, up (r, c) xs, down (r, c) xs]

right :: (Int, Int) -> [[Int]] -> Maybe Int
right (r, c) xs = if c < length (xs !! r) - 1 then Just ((xs !! r) !! (c+1)) else Nothing

left :: (Int, Int) -> [[Int]] -> Maybe Int
left (r, c) xs = if c > 0 then Just ((xs !! r) !! (c-1)) else Nothing

up :: (Int, Int) -> [[Int]] -> Maybe Int
up (r, c) = left (c, r) . transpose

down :: (Int, Int) -> [[Int]] -> Maybe Int
down (r, c) = right (c, r) . transpose

findAllBasins :: [[Int]] -> [Maybe Int]
findAllBasins = undefined

getBasinSize :: (Int, Int) -> [[Int]] -> Int
getBasinSize coords xs = undefined

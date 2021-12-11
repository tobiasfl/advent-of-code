module Day9 where

import System.Environment   
import Data.List
import Data.Char(digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
--import Control.Applicative
import Data.Maybe
import Data.Tuple

solveBoth :: IO ()
solveBoth = do
    contents <- lines <$> readFile "./infiles/Day9.in"
    --contents <- lines <$> readFile "./infiles/Day9Test.in"
    print $ solveA $ map (map digitToInt) contents

parseMap :: [[Int]] -> Map (Int, Int) Int
parseMap xs = Map.fromList [((r, c), (xs !! r) !! c) | r <- [0..length xs-1], c <- [0..length (transpose xs)-1]]

solveA :: [[Int]] -> Int
solveA xs = sum $ map (+1) $ catMaybes $ lowPoints (parseMap xs) (allCoords xs)

allCoords :: [[Int]] -> [(Int, Int)]
allCoords xs = [(r, c) | r <- [0..length xs-1], c <- [0..length (transpose xs)-1]]

neighbors :: (Int, Int) -> Map (Int, Int) Int -> [Int]
neighbors (r, c) m =  mapMaybe (`Map.lookup` m) [(r, c+1), (r, c-1), (r+1, c), (r-1, c)]

maybeLowPoint :: (Int, Int) -> Map (Int, Int) Int -> Maybe Int
maybeLowPoint coords m = (\neighs -> Map.lookup coords m >>= \x -> if all (x<) neighs then Just x else Nothing) $ neighbors coords m

lowPoints :: Map (Int, Int) Int -> [(Int, Int)] -> [Maybe Int]
lowPoints m = map (`maybeLowPoint` m) 

bfs :: (Int, Int) -> Map (Int, Int) a -> a
bfs = undefined

findAllBasins :: [[Int]] -> [Maybe Int]
findAllBasins = undefined

getBasinSize :: (Int, Int) -> [[Int]] -> Int
getBasinSize coords xs = undefined

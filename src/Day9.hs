module Day9 where

import Lib(mkGridMap, bfs, allCoords, neighborVals, GridMap)
import System.Environment   
import Data.List
import Data.Char(digitToInt)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Tuple

solveBoth :: IO ()
solveBoth = do
    contents <- lines <$> readFile "./infiles/Day9.in"
    --contents <- lines <$> readFile "./infiles/Day9Test.in"
    print $ solveA $ map (map digitToInt) contents

solveA :: [[Int]] -> Int
solveA xs = sum $ map (+1) $ catMaybes $ lowPoints (mkGridMap xs) (allCoords xs)

maybeLowPoint :: (Int, Int) -> GridMap Int -> Maybe Int
maybeLowPoint coords m = (\neighs -> Map.lookup coords m >>= \x -> if all (x<) neighs then Just x else Nothing) $ neighborVals coords m

lowPoints :: GridMap Int -> [(Int, Int)] -> [Maybe Int]
lowPoints m = map (`maybeLowPoint` m) 

solveB :: [[Int]] -> Int
solveB = undefined

--TODO: implement bfs in Lib.hs and use here
findAllBasins :: [[Int]] -> [Maybe Int]
findAllBasins = undefined

getBasinSize :: (Int, Int) -> [[Int]] -> Int
getBasinSize coords xs = undefined

module Day11 where

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

solveBoth :: IO ()
solveBoth = do
    grid <- mkGridMap . map (map digitToInt) . lines <$> readFile "./infiles/Day11Test.in"
    print $ solveA 5 grid
    print $ solveA 100 grid
    print $ take 10 $ head $ map (Map.elems . fst) $ drop 1 $ iterate (\(m, count) -> (\flashed -> (resetFlashed flashed, flashesAfterStep flashed)) $ shine $ increaseEnergy m) (grid, 0) 
    print $ take 10 $ head $ map (Map.elems . fst) $ drop 2 $ iterate (\(m, count) -> (\flashed -> (resetFlashed flashed, flashesAfterStep flashed)) $ shine $ increaseEnergy m) (grid, 0) 
    print $ take 10 $ head $ map (Map.elems . fst) $ drop 3 $ iterate (\(m, count) -> (\flashed -> (resetFlashed flashed, flashesAfterStep flashed)) $ shine $ increaseEnergy m) (grid, 0) 


solveA :: Int -> GridMap Int -> Int
solveA steps gm = sum $ map snd $ take steps results
    where results = iterate (\(m, count) -> (\flashed -> (resetFlashed flashed, flashesAfterStep flashed)) $ shine $ increaseEnergy m) (gm, 0) 

increaseEnergy :: GridMap Int -> GridMap Int
increaseEnergy = Map.map (+1) 

--updates map and subsequent flashes until done
shine :: GridMap Int -> GridMap Int
shine m = shineStep (Map.map (\v -> if v == 10 then 11 else v) m) (Map.foldrWithKey (\k v a -> if v == 10 then neighborCoords8 k ++ a else a) [] m)

shineStep :: GridMap Int -> [(Int, Int)] -> GridMap Int
shineStep gm [] = gm
shineStep gm cs = shineStep updatedGm (Map.foldrWithKey (\k v a -> if v == 10 then neighborCoords8 k ++ a else a) [] updatedGm)
    where updatedGm = foldr (Map.adjust (+1)) gm cs


filteredNeighbors :: GridMap Int -> (Int, Int) -> [(Int, Int)]
filteredNeighbors m = filter f . neighborCoords8 
    where f c = case Map.lookup c m of Just v  -> v < 10
                                       Nothing -> False

flashesAfterStep :: GridMap Int -> Int
flashesAfterStep = Map.foldr (\v -> if v > 9 then (+1) else (+0)) 0

resetFlashed :: GridMap Int -> GridMap Int
resetFlashed = Map.map (\x -> if x > 9 then 0 else x) 




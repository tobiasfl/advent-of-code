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
    grid <- mkGridMap . map (map digitToInt) . lines <$> readFile "./infiles/Day11.in"
    print $ solveA 100 grid

solveA :: Int -> GridMap Int -> Int
solveA steps gm = sum $ map snd $ take (1+steps) results
    where results = iterate (\(m, count) -> step m) (gm, 0) 

solveB :: Int -> GridMap Int -> Int
solveB = undefined

step :: GridMap Int -> (GridMap Int, Int)
step gm = let beforeReset = flash $ Map.map (+1) gm
        in (resetFlashed beforeReset, flashesAfterStep beforeReset)

--finds which first need flashing, then starts cycle of flash steps
flash :: GridMap Int -> GridMap Int
flash m = flashStep m (toFlash m)
    where toFlash = Map.foldrWithKey 
              (\k v a -> if v == 10 then k:neighborCoords8 k ++ a else a) [] 

--flash octopuses while gathering up any to be flashed next
flashStep :: GridMap Int -> [(Int, Int)] -> GridMap Int
flashStep gm [] = gm
flashStep gm cs = uncurry flashStep updatedGm
    where updatedGm = foldr go (gm, []) cs
          go c (m, xs) = fromMaybe (m, xs) (do
              v <- Map.lookup c m
              let newMap = Map.adjust (+1) c m 
              let newList = if v+1 == 10 then c:neighborCoords8 c ++ xs else xs 
              return (newMap, newList))

          

flashesAfterStep :: GridMap Int -> Int
flashesAfterStep = Map.foldr (\v -> if v > 9 then (+1) else (+0)) 0

resetFlashed :: GridMap Int -> GridMap Int
resetFlashed = Map.map (\v -> if v > 9 then 0 else v) 




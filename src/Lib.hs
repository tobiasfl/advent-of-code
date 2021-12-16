module Lib
    ( someFunc,
      mkGridMap,
      allCoords,
      neighborVals,
      GridMap,
      neighborCoords,
      neighborCoords8
    ) where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type GridMap a = Map (Int, Int) a

mkGridMap :: [[a]] -> GridMap a
mkGridMap xs = Map.fromList [((r, c), (xs !! r) !! c) | r <- [0..length xs-1], c <- [0..length (transpose xs)-1]]

neighborVals :: (Int, Int) -> GridMap a -> [a]
neighborVals (r, c) m =  mapMaybe (`Map.lookup` m) $ neighborCoords (r, c) 

neighborCoords :: (Int, Int) -> [(Int, Int)]
neighborCoords (r, c) = [(r, c+1), (r, c-1), (r+1, c), (r-1, c)]

neighborCoords8 :: (Int, Int) -> [(Int, Int)]
neighborCoords8 (r, c) = [(r-1, c-1), (r-1, c+1), (r+1, c-1), (r+1, c+1)] ++ neighborCoords (r, c)

allCoords :: [[Int]] -> [(Int, Int)]
allCoords xs = [(r, c) | r <- [0..length xs-1], c <- [0..length (transpose xs)-1]]



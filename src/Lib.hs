module Lib
    ( someFunc,
      mkGridMap,
      allCoords,
      neighborVals,
      GridMap,
      neighborCoords,
      neighborCoords8,
      binToInt,
      intToBin
    ) where

import Data.Char(digitToInt, intToDigit)
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

binToInt :: String -> Int
binToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

intToBin :: Int -> String
intToBin 0 = "0"
intToBin n = map intToDigit $ reverse (helper n)
    where helper 0 = []
          helper n = let (q,r) = n `divMod` 2 in r : helper q

type Graph a b = Map a b




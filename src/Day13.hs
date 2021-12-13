module Day13 where

import Lib(mkGridMap, bfs, allCoords, GridMap, neighborCoords8)
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
    grid <- map (map digitToInt) . lines <$> readFile "./infiles/Day12.in"
    putStrLn "huda"


{-# LANGUAGE TemplateHaskell   #-}

module AoC2022.Day12 where

import Data.Char (ord)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Multimap (Multimap)
import qualified Data.Multimap as MM
import Data.Maybe (fromMaybe, mapMaybe)
import Control.Monad.State
import Control.Lens.TH (makeLenses)
import Control.Lens ((%~),(.~))

--current pos S
--best signal pos E
--elevation levels go from a..zip 
--only nodes with an elevation difference of 1 may be connected(ie. you can only move 1 elevation higher)

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day12.in"
    print $ parseInput fileContents

parseInput :: String -> Grid
parseInput = Map.fromList . concat . zipWith (\x -> zip [(x,y) | y <- [0..]]) [0..] . lines

type Grid = Map Pos Char

type Pos = (Int, Int)

data StateBFS = StateBFS
    { _grid :: Grid
    , _visited :: Set Pos
    , _paths :: [[Pos]]
    , _currPath :: [Pos]--TODO: might want to use more efficient queue if necessary
    , _endGoal :: Pos
    }

makeLenses ''StateBFS

goal :: Grid -> Pos
goal = undefined

start :: Grid -> Pos
start = undefined

neighbors :: Pos -> Grid -> [Pos]
neighbors (x, y) g = filter (\p -> maybe False (closeAltitude currChar) (Map.lookup p g)) coords
    where coords = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
          currChar = fromMaybe 'a' $ Map.lookup (x, y) g
--TODO: want key as well...


closeAltitude :: Char -> Char -> Bool
closeAltitude x y = maximum asInts - minimum asInts <= 1
    where asInts = map ord [x, y]

allPaths :: Pos -> Pos -> Grid -> [[Pos]]
allPaths = undefined

--bfs :: Pos -> State StateBFS ()
--bfs curr = do
--    modify (visited %~ Set.insert curr)
--    modify (paths %~ ([curr]:))
--    
--    
--
--processPath :: [Pos] -> State StateBFS ()
--processPath path = do
--    put (currPath .~ path)





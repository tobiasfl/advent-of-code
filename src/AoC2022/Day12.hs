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
import Control.Monad (unless, when, forM_)
import Control.Lens.TH (makeLenses)
import Control.Lens.Setter ((%~),(.~))
import Control.Applicative (liftA2)
import Debug.Trace
import Data.Functor((<&>))
import Data.List (find)
--current pos S
--best signal pos E
--elevation levels go from a..zip 
--only nodes with an elevation difference of 1 may be connected(ie. you can only move 1 elevation higher)

type Grid = Map Pos Char

type Pos = (Int, Int)

data StateBFS = StateBFS
    { _grid :: Grid
    , _queue :: [[Pos]]--TODO: would be easier to work with proper queue structure
    , _endGoal :: Pos
    , _paths :: [[Pos]]
    }

makeLenses ''StateBFS

--TODO: rewrite with more appropriate lens stuff (e.g. composing them)

goal :: Grid -> Pos
goal g = fromMaybe (0, 0) $ find ((Just 'E'==) . (`Map.lookup` g)) $ Map.keys g

start :: Grid -> Pos
start g = fromMaybe (0, 0) $ find ((Just 'S'==) . (`Map.lookup` g)) $ Map.keys g

neighbors :: Pos -> Grid -> [Pos]
neighbors p@(x, y) g = filter (fromMaybe False . liftA2 closeAltitude currChar . (`Map.lookup` g)) coords
    where coords = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
          currChar = Map.lookup p g

closeAltitude :: Char -> Char -> Bool
closeAltitude x y = maximum asInts - minimum asInts <= 1
    where asInts = map ord [x, y]

popQueue :: State StateBFS [Pos]
popQueue = do
   path <- gets (head . _queue)
   modify' (queue %~ drop 1)
   return path

bfs :: Pos -> State StateBFS ()
bfs start = do
    modify' (queue %~ ([start]:))
    processQueue

processQueue :: State StateBFS ()
processQueue = do
    q <- gets _queue
    trace (show q) (return ())
    unless (null q) (do
        trace "inloop" (return ())
        path <- popQueue
        let lastInPath = last path
        end <- gets _endGoal

        when (lastInPath == end) (modify' (paths %~ (path:)))

        grid <- gets _grid
        forM_ (neighbors lastInPath grid) (\p -> do
            unless (p `elem` path) (do
                let newPath = p:path
                modify' (queue %~ (++[newPath]))))

        processQueue)

parseInput :: String -> Grid
parseInput = Map.fromList . concat . zipWith (\x -> zip [(x,y) | y <- [0..]]) [0..] . lines

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day12.in"
    let grid = parseInput fileContents
    print $ "result:" ++  show ( _paths $ execState (bfs (start grid)) (StateBFS grid [] (goal grid) []))

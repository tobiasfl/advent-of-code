module AoC2021.Day15 where

import Lib(mkGridMap, neighborCoords, allCoords, GridMap)
import Data.List.Split(splitOn)
import System.Environment   
import Data.List
import Data.Char(digitToInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State
import Data.Bifunctor(first, second)
import Control.Monad (forM_)

solveBoth :: IO ()
solveBoth = do
    (graph, dst) <- parseInput . lines <$> readFile "./infiles/Day15Test.in"
    print dst
    print $ evalState (dijkstra graph) (graph, S.empty)
    print $ solveA (graph, dst)

--To store current distance from start to other nodes
type CostMap = GridMap Int

--Set of nodes whose min distance from start is calculated
type MinSet = Set (Int, Int)

solveA :: (GridMap Int, (Int, Int)) -> Int
--no result....
solveA (graph, dst) = fromJust $ M.lookup dst $ evalState (dijkstra graph) (graph, S.empty)

start :: (Int, Int)
start = (0, 0)

dijkstra :: GridMap Int -> State (CostMap, MinSet) CostMap
dijkstra graph = do
    --set all to infinity
    modify (first (M.map (const maxBound)))
    --set start node to 0 so it is chosen first
    modify (first (M.insert start 0))
   
    -- for every node
    forM_ (map fst $ M.toList graph) (\curr -> do
        (costs, minSet) <- get
        let minNode = minDist minSet costs
        modify (second (S.insert minNode))
        updateCosts minNode graph)
    (costs, ms) <- get
    return costs


--find the node with cheapest cost that is also not in the minimum set
minDist :: MinSet -> CostMap -> (Int, Int)
minDist ms = fst . minimumBy (\(k, v) (k1, v1) -> compare v v1) . M.toList . M.filterWithKey (\k v -> S.notMember k ms)
    
--update neighbor values of closest node if current dist is greater than new distance
--TODO: clean it up!
    --BUG HERE....
updateCosts :: (Int, Int) -> GridMap Int -> State (CostMap, MinSet) ()
updateCosts minNode graph = do
    (costs, minSet) <- get
    let neighs = mapMaybe (\k -> M.lookup k graph >>= \v -> Just (k, v)) $ neighborCoords minNode
    let filteredNeighs = filter (\(k, v) -> v > 0 && start /= k && S.notMember k minSet) neighs
    forM_ filteredNeighs (\(currK, currV) -> do
        -- shoould be cost from minNode to curr
        let minCost = fromJust (M.lookup minNode costs) + currV
        let newCosts = foldr (\(k, v) cs -> if v > minCost then M.adjust (const minCost) k cs else cs) costs filteredNeighs
        modify (first (const newCosts)))

--Returns graph and end node, assume quadratic grid
parseInput :: [String] -> (GridMap Int, (Int, Int))
parseInput xs = (mkGridMap $ map (map digitToInt) xs, (length xs-1, length xs-1))

module Day12 where

import Data.List.Split(splitOn)
import System.Environment   
import Data.List
import Data.Char(digitToInt, isUpper)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Monad.State
import Data.Bifunctor(first)


solveBoth :: IO ()
solveBoth = do
    graph <- parseInput . lines <$> readFile "./infiles/Day12.in"
    testGraph <- parseInput . lines <$> readFile "./infiles/Day12Test.in"
    print $ solveA testGraph
    --print $ solveA graph
    



type Graph = Map String [String]

solveA :: Graph -> Int
solveA graph = evalState (checkPaths graph start) (Map.fromList [(x, 0) | x <- Map.keys graph], 0)

start = "start"

end = "end"

checkPaths :: Graph -> String -> State (Map String Int, Int) Int
checkPaths graph src = do
    --mark as visited
    modify (first (Map.adjust (+1) src))  

    --if it is the end increment count
    when (src == end) (modify (\(v, pc) -> (v, pc+1)))

    --check neighbors
    when (src /= end) $ do
        (visited, pathCount) <- get
        let neighs = fromMaybe [] $ Map.lookup src graph
        mapM_ (checkPaths graph) $ filterNeighsB visited neighs

    --mark as non- visited again
    modify (first (Map.adjust (\n -> n-1) src)) 

    (visited, pathCount) <- get
    return pathCount 

filterNeighsA :: Map String Int -> [String] -> [String]
filterNeighsA visited = filter (\n -> all isUpper n || Map.findWithDefault 0 n visited == 0) 

--TODO: unfinished
filterNeighsB :: Map String Int -> [String] -> [String]
filterNeighsB visited = filter (\n -> all isUpper n || (\vc -> (n == start && vc < 0) || (n /= start && vc < 2)) (Map.findWithDefault 0 n visited))

parseInput :: [String] -> Graph
parseInput = Map.fromListWith (++) . concatMap (edge . splitOn "-")
    where edge [x, y] = [(x, [y]), (y, [x])]
          edge _ = []

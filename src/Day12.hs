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
    print graph
    print $ solveA testGraph
    print $ solveA graph


type Graph = Map String [String]

solveA :: Graph -> Int
solveA graph = evalState (checkPaths graph "start") ([], 0)

start :: String
start = "start"

end :: String
end = "end"

--TODO: could use Map of visited and times they are visited instead of list to make lookups more efficient???
--State with type of state being set of seen and count of paths being the produced result
--string is start
checkPaths :: Graph -> String -> State ([String], Int) Int
checkPaths graph src = do
    --mark as visited
    modify (first (src:))  

    --if it is the end increment count
    when (src == end) (modify (\(v, pc) -> (v, pc+1)))
    
    when (src /= end) $ checkNeighbors graph src
    modify (first (filter (/=src))) 

    (visited, pathCount) <- get
    return pathCount 

checkNeighbors :: Graph -> String -> State ([String], Int) ()
checkNeighbors graph src = do
    (visited, pathCount) <- get
    let neighs = fromMaybe [] $ Map.lookup src graph
    mapM_ (checkPaths graph) $ filterNeighsA visited neighs
   

filterNeighsA :: [String] -> [String] -> [String]
filterNeighsA visited = filter (\n -> all isUpper n || notElem n visited)

--TODO: unfinished
filterNeighsB :: [String] -> [String] -> [String]
filterNeighsB visited = filter (\n -> all isUpper n || (length (filter (n==) visited) < 2))

parseInput :: [String] -> Graph
parseInput = Map.fromListWith (++) . concatMap (edge . splitOn "-")
    where edge [x, y] = [(x, [y]), (y, [x])]
          edge _ = []

module AoC2021.Day9 where

import Lib(mkGridMap, allCoords, neighborVals, GridMap, neighborCoords)
import System.Environment   
import Data.List
import Data.Char(digitToInt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import Data.Tuple

solveBoth :: IO ()
solveBoth = do
    graph <- map (map digitToInt) . lines <$> readFile "./infiles/Day9.in"
    print $ solveA graph
    print $ solveB graph

solveA :: [[Int]] -> Int
solveA xs = sum $ map ((+1) . snd) $ catMaybes $ lowPoints (mkGridMap xs) (allCoords xs)

maybeLowPoint :: (Int, Int) -> GridMap Int -> Maybe ((Int, Int), Int)
maybeLowPoint coords m = M.lookup coords m >>= \x -> 
    if all (x<) (neighborVals coords m) then Just (coords, x) else Nothing

lowPoints :: GridMap Int -> [(Int, Int)] -> [Maybe ((Int, Int), Int)]
lowPoints m = map (`maybeLowPoint` m) 

solveB :: [[Int]] -> Int
solveB xs = topThreeBasinProd $ findBasins $ catMaybes $ lowPoints graph (allCoords xs)
    where graph = mkGridMap xs
          findBasins = map ((\lp -> bfs graph [lp] $ S.singleton lp) . fst)
          topThreeBasinProd = product . take 3 . reverse . sort . map length
         
--finds set of coords related to each basin by doing bfs from a low point
bfs :: GridMap Int -> [(Int, Int)] -> Set (Int, Int) -> Set (Int, Int)
bfs _ [] discovered = discovered
bfs graph (curr:xs) discovered = bfs graph newQueue newVisited
    where relevantNeighbors = filter isRelevantPred $ neighborCoords curr
          isRelevantPred c = maybe False (<9) (M.lookup c graph) && S.notMember c discovered
          newQueue = xs ++ relevantNeighbors 
          newVisited = discovered `S.union` S.fromList relevantNeighbors


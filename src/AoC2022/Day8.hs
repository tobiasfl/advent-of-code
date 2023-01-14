module AoC2022.Day8 where

import Data.Maybe (mapMaybe, fromMaybe, isJust, catMaybes)
import Data.Char (digitToInt)
import Data.Map (Map)
import Data.List (transpose, nub, mapAccumL)
import qualified Data.Map as M
import Text.Read (readMaybe)
import Data.Bifunctor (bimap, second, first)

type Coord = (Int, Int)

type Dir = (Int, Int)

solve :: IO ()
solve = do
    parsed <- parseInput <$> readFile "./infiles/AoC2022/Day8.in"
    print $ visibleTrees parsed
    print $ highestScenicScore parsed

parseInput :: String -> [[(Coord, Int)]]
parseInput = zipWith (\x -> zip [(x,y) | y <- [0..]]) [0..] . asInts
    where asInts = map (map digitToInt) . lines

visibleTrees :: [[(Coord, Int)]] -> Int
visibleTrees = length . checkFromAllSides

checkFromAllSides :: [[(Coord, Int)]] -> [(Coord, Int)]
checkFromAllSides g =
    nub (concatMap (concatMap (\r -> checkRow r ++ checkRow (reverse r))) sides)
        where sides = [g, transpose g]

checkRow :: [(Coord, Int)] -> [(Coord, Int)]
checkRow = map snd . filter fst . snd . mapAccumL mapper (-1)
    where mapper highest t@(c, tHeight) = (max highest tHeight, (highest < tHeight, t))

highestScenicScore :: [[(Coord, Int)]] -> Int
highestScenicScore g = maximum $ map (`treeScenicScore` asMap) (concat g)
    where asMap = M.fromList $ concat g

treeScenicScore :: (Coord, Int) -> Map Coord Int -> Int
treeScenicScore ((x, y), t) gm = product sides
        where sides = [checkDir (second (+1)),
                       checkDir (second (subtract 1)),
                       checkDir (first (+1)),
                       checkDir (first (subtract 1))]
              checkDir :: (Coord -> Coord) -> Int
              checkDir dir = visibleTrees (iterate dir (dir (x, y)))
              visibleTrees :: [Coord] -> Int
              visibleTrees [] = 0
              visibleTrees (c:xs) = case c `M.lookup` gm of 
                               Just th -> if th < t then 1 + visibleTrees xs else 1
                               Nothing -> 0

--1843
--180000

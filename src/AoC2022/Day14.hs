{-# LANGUAGE TupleSections #-}

module AoC2022.Day14 where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe, catMaybes, isJust, listToMaybe)
import Data.List (concatMap, sort)
import Data.List.Extra (splitOn, headDef, lastDef)
import Text.Read (readMaybe)
import Debug.Trace (trace, traceShow)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Bifunctor (second)

--Sand is produced one unit at a time, and the next unit of sand is not produced until the previous unit of sand comes to rest

--Sand keeps moving as long as it is able to do so, at each step trying to:
--move down, 
--then down-left, 
--then down-right.

--The output of the example should be like this when prettyprinted:
-- .......+...
-- .......~...
-- ......~o...
-- .....~ooo..
-- ....~#ooo##
-- ...~o#ooo#.
-- ..~###ooo#.
-- ..~..oooo#.
-- .~o.ooooo#.
-- ~#########.
-- ~..........
-- ~..........
-- ~..........

data Unit = Sand | Stone
    deriving (Show, Eq)

type Coord = (Int, Int)

type Grid = Map Coord Unit

sandStart :: Coord
sandStart = (500, 0)

restingSandCount :: Grid -> Int
restingSandCount = length . M.filter (Sand==) . restingSandGrid

restingSandGrid :: Grid -> Grid
restingSandGrid = lastDef M.empty . catMaybes . takeWhile isJust . iterate sandFall . Just


sandFall :: Maybe Grid -> Maybe Grid
sandFall (Just g) = findRestCoord sandStart g >>= \rc -> Just $ M.insert rc Sand g
sandFall _ = Nothing


--Make nextBelow faster
findRestCoord :: Coord -> Grid -> Maybe Coord
findRestCoord start g = do
           current@(xC, yC) <- nextBelow start
           guard (yC >= snd start && yC > snd sandStart)
           checkFrom (xC-1, yC+1) <|> checkFrom (xC+1, yC+1) <|> pure current
    where nextBelow (x, y) =
            let exes = (map snd $ filter ((x==) . fst) $ M.keys g)
             in (x,) . subtract 1 <$> if null exes then Nothing else Just $ minimum exes
          checkFrom coord = if coord `M.notMember` g
                                then findRestCoord coord g <|> (M.lookup (second (+1) coord) g >> pure coord)
                                else Nothing

example = "498,4 -> 498,6 -> 496,6\n503,4 -> 502,4 -> 502,9 -> 494,9"

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day14.in"
    let g = parseInput fileContents
    --let g = parseInput example
    putStrLn $ prettyPrint g
    putStrLn $ prettyPrint (restingSandGrid g)
    print $ restingSandCount g

prettyPrint :: Grid -> String
prettyPrint g = unlines $ [[coordToChar (x, y) | x <- [minX..maxX]] | y <- [0..maxY]]
    where minX = minimum $ map fst $ M.keys g
          maxX = maximum $ map fst $ M.keys g
          maxY = maximum $ map snd $ M.keys g
          coordToChar c = maybe '.' (\e -> if e == Sand then 'o' else '#') $ M.lookup c g

parseInput :: String -> Grid
parseInput = toGrid . concatMap (fillLines . map toCoord . splitOn " -> ") . lines
    where toCoord = (\xs -> (headDef 0 xs, lastDef 0 xs)) <$> mapMaybe readMaybe . splitOn ","
          fillLines ((x, y):n@(x2, y2):xs) = zip (range x x2) (range y y2) ++ fillLines (n:xs)
          fillLines xs = xs
          range a b = [a, a + signum (b-a)..b]
          toGrid = M.fromList . flip zip (repeat Stone)

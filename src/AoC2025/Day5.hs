module AoC2025.Day5 where

import Text.Read (readMaybe)
import qualified Data.List (filter)
import Control.Applicative (Applicative(liftA2))
import Data.List.Extra
import Data.List (union)
import Data.Maybe
import Debug.Trace (trace, traceShow)

example :: String
example = "3-5\n10-14\n16-20\n12-18"

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2025/Day5.in"
    let (freshIdRanges, availables) = parse fileContents
    print $ length $ freshIds freshIdRanges availables
    print $ allFreshIdsCount freshIdRanges

parse :: String -> ([(Int, Int)], [Int])
parse input = (freshRanges, availables)
    where (firstPart, secondPart) = breakOn "\n\n" input
          freshRanges = mapMaybe (parseTuple . splitOn "-") $ lines firstPart
          parseTuple [start, stop] = liftA2 (,) (readMaybe start) (readMaybe stop)
          parseTuple _ = Nothing
          availables = mapMaybe readMaybe $ lines secondPart

freshIds :: [(Int, Int)] -> [Int] -> [Int]
freshIds allFreshIds = filter (\toCheck -> any (isInRange toCheck) allFreshIds)
    where isInRange toCheck (s, e) = toCheck >= s && toCheck <= e 

allFreshIdsCount :: [(Int, Int)] -> Int
allFreshIdsCount = count . sort
    where count (l@(ls, le):r@(rs, re):xs)
            | ls <= rs && le >= rs = rs - ls + count ((rs, max le re):xs)
            | otherwise = 1 + le - ls + count (r:xs)
          count [(s, e)] = 1 + e - s
          count _ = 0

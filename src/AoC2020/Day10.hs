module AoC2020.Day10 where

import Data.Set (Set)
import qualified Data.Set as S
import Text.Read (readMaybe)
import Data.List (group,sort,mapAccumL)
import Control.Monad.State

--input: list of joltage adapters, each one can take input 1-3 jolts lower than its rating
--device: rated for 3 jolts higher than the highest rated adapter
--charging outlet: 0 jolts effective rating

--If you use every adapter in the bag: 

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2020/Day10.in"
    linesAsInts <- maybe (fail "parse error") return $ parseInput fileContents
    print $ product $ diffCounts linesAsInts

parseInput :: String -> Maybe [Int]
parseInput = traverse readMaybe . lines

diffs :: [Int] -> [Int]
diffs = (3:) . snd . mapAccumL (\jolts adaptJolts -> (adaptJolts, adaptJolts-jolts)) 0 . sort

diffCounts :: [Int] -> [Int]
diffCounts = fmap length . group . sort . diffs

--TODO: Should use backtracking in part 2

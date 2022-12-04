module AoC2022.Day3 where

import Data.List (intersect, nub)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

solve :: IO ()
solve = do
    fileContents <- lines <$> readFile "./infiles/AoC2022/Day3.in"
    print $ commonItemPriorities fileContents
    print $ badgePriorities fileContents

commonItemPriorities :: [String] -> Int
commonItemPriorities = sum . map (itemPriority . findCommonItem)

itemPriority :: Char -> Int
itemPriority = fromMaybe 0 . (`M.lookup` prioMap)
    where prioMap = M.fromList $ zip (['a'..'z'] ++ ['A'..'Z']) [1..52]

findCommonItem :: String -> Char
findCommonItem xs = head $ nub $ uncurry intersect $ splitAt (length xs `div` 2) xs

badgePriorities :: [String] -> Int
badgePriorities = sum . concatMap (map itemPriority) . combineGroups

combineGroups :: [String] -> [String]
combineGroups (x:y:z:xs) = foldl (intersect . nub) x [x,y,z]:combineGroups xs
combineGroups xs = xs

module AoC2020.Day3 where

import Data.Either

solveBoth :: IO ()
solveBoth = do
    grid <- fmap cycle . lines <$> readFile "./infiles/AoC2020/Day3.in"
    print $ "A:" ++ show (countTrees (1, 3) grid)
    let b = product $ fmap (`countTrees` grid) [(1, 1), (1, 3), (1, 5), (1, 7), (2, 1)]
    print $ "B:" ++ show b

countTrees :: (Int, Int) -> [String] -> Int
countTrees (rows, cols) grid = foldr (\c acc -> if isTree c then acc+1 else acc) 0 allStops
    where allStops = zip [0,rows..length grid-1] [0,cols..]
          isTree (r, c) = '#' == ((grid !! r) !! c)

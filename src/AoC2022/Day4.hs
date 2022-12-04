module AoC2022.Day4 where

import Text.Parsec as Parsec
import Data.Either
import Data.Set (Set)
import qualified Data.Set as S

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day4.in"
    print $ countPairsWithSubset $ parseInput fileContents
    print $ countPairsWithOverlap $ parseInput fileContents

parseInput :: String -> [(Set Int, Set Int)]
parseInput = map (fromRight (S.empty, S.empty) . parse rangePairParser "") . lines
    where rangePairParser = do 
            s1 <- read <$> manyTill digit (char '-')
            e1 <- read <$> manyTill digit (char ',')
            s2 <- read <$> manyTill digit (char '-')
            e2 <- read <$> many1 digit 
            return (S.fromAscList [s1..e1], S.fromAscList [s2..e2])

countPairsWithSubset :: [(Set Int, Set Int)] -> Int
countPairsWithSubset = length . filter pairHasSubset

pairHasSubset :: (Set Int, Set Int) -> Bool
pairHasSubset (set1, set2) = set1 `S.isSubsetOf` set2 || set2 `S.isSubsetOf` set1

countPairsWithOverlap :: [(Set Int, Set Int)] -> Int
countPairsWithOverlap = length . filter pairOverlaps

pairOverlaps :: (Set Int, Set Int) -> Bool
pairOverlaps (set1, set2) = not $ S.null $ set1 `S.intersection` set2

{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module AoC2020.Day9 where

import Data.Multimap (Multimap)
import qualified Data.Multimap as MM
import Data.Array
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Monad.Reader
import Debug.Trace
import Data.List (subsequences, find)

data Data = Data
    { line2Num :: Array Int Int
    , num2Line :: Multimap Int Int
    }

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2020/Day9.in"
    linesAsInts <- maybe (fail "parse error") return $ parseInput fileContents
    let line2num = listArray (0, length linesAsInts-1) linesAsInts
    let num2lines = MM.fromList $ zip linesAsInts [0..length linesAsInts-1]
    let invalidNum = runReader firstInvalidNum (Data line2num num2lines)
    print invalidNum
    print $ findMinAndMaxNum linesAsInts invalidNum
    let addedMinAndMax = (case findMinAndMaxNum linesAsInts invalidNum of Just (x, y) -> x+y
                                                                          Nothing -> 0) 
    print addedMinAndMax

parseInput :: String -> Maybe [Int]
parseInput = traverse readMaybe . lines

rangeToCheck :: Int -> [Int]
rangeToCheck line = [line-1, line-2..line-25]

firstInvalidNum :: Reader Data Int
firstInvalidNum = do
    l2n <- asks line2Num
    validLines <- takeWhile id <$> traverse lineIsValid [25..length l2n]
    return $ l2n ! (length validLines + 25)

lineIsValid :: Int -> Reader Data Bool
lineIsValid line = do
    l2n <- asks line2Num
    or <$> traverse (tryToSumWithLine (l2n ! line)) (rangeToCheck line)

tryToSumWithLine :: Int -> Int -> Reader Data Bool
tryToSumWithLine num line = do
    (Data l2n n2l) <- ask
    let numOnLine = l2n ! line
    let numNeededForSum = max numOnLine num - min numOnLine num
    return $ any (`elem` rangeToCheck line) (MM.lookup numNeededForSum n2l)

findMinAndMaxNum :: [Int] -> Int -> Maybe (Int, Int)
findMinAndMaxNum xs = fmap (\(minIndex, maxIndex) -> (xs !! minIndex, xs !! (maxIndex-1))) . findRange xs 

findRange :: [Int] -> Int -> Maybe (Int, Int)
findRange xs numToFind = 
    find (\(start, end) -> numToFind == pSums !! end - pSums !! (start-1)) (rangesToCheck xs)
    where pSums = partialSums xs

rangesToCheck :: [Int] -> [(Int, Int)]
rangesToCheck xs = do
    x <- indices
    y <- indices
    guard (x < y)
    [(x, y)]
        where indices = [1..length xs]

partialSums :: [Int] -> [Int]
partialSums = scanl (+) 0 

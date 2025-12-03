

module AoC2024.Day5 where

import Debug.Trace (trace)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC
import Text.Read (readMaybe)
import qualified Data.Set as S
import Data.Maybe (catMaybes, mapMaybe)
import Data.List

type PageOrdering = (Int, Int)

type Update = [Int]

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2024/Day5.in"
    (orderings, updates) <- either (fail . show) pure $ P.parse inputParser "" fileContents
    print $ sum $ middleNums $ orderedUpdates orderings updates
    print $ sum $ middleNums $ map (reOrder orderings) $ disorderedUpdates orderings updates

inputParser :: P.Parsec String () (S.Set PageOrdering, [Update])
inputParser = do
    orderings <- P.manyTill pageOrdering (PC.string "\n")
    updates <- P.manyTill update P.eof
    pure (S.fromList orderings, updates)

pageOrdering :: P.Parsec String () PageOrdering
pageOrdering = do
    firstNum <- intParser
    P.char '|'
    secondNum <- intParser
    P.char '\n'
    pure (firstNum, secondNum)

update :: P.Parsec String () Update
update = do
    ints <- intParser `P.sepBy` P.char ','
    P.char '\n'
    pure ints
    
intParser :: P.Parsec String () Int 
intParser = P.count 2 PC.digit >>= maybe (fail "int parse failed") pure . readMaybe

middleNums :: [Update] -> [Int]
middleNums = map (\xs -> xs !! (length xs `div` 2))

orderedUpdates :: S.Set PageOrdering -> [Update] -> [Update]
orderedUpdates pOrds = filter (isCorrectOrder pOrds)

isCorrectOrder :: S.Set PageOrdering -> Update -> Bool
isCorrectOrder _ [] = True
isCorrectOrder pOrds (x:xs) = all (\y ->  (y, x) `S.notMember` pOrds) xs && isCorrectOrder pOrds xs

disorderedUpdates :: S.Set PageOrdering -> [Update] -> [Update]
disorderedUpdates pOrds updates = filter (not . (`elem` ordereds)) updates
    where ordereds = orderedUpdates pOrds updates

reOrder :: S.Set PageOrdering -> Update -> Update
reOrder pOrds  = sortBy (\ x y -> if (y, x) `S.member` pOrds then GT else LT)

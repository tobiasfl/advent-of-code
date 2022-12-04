module AoC2022.Day2 where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as M

data Type = Rock | Paper | Scissors
    deriving (Show, Eq, Ord)

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day2.in"
    print $ totalScore $ parse fileContents
    print $ partTwoTotalScore $ parse fileContents


parse :: String -> [[Type]]
parse = fmap (mapMaybe mapToType) . lines
    where mapToType k = M.lookup k typeMapping
          typeMapping = M.fromList [('A',Rock), ('B',Paper),('C',Scissors),('X',Rock), ('Y',Paper),('Z',Scissors)]

totalScore :: [[Type]] -> Int
totalScore = sum . fmap roundScore

roundScore :: [Type] -> Int
roundScore (opp:me:xs) = fromMaybe 0 $ M.lookup (opp, me) scoreMapping
roundScore xs = 0

scoreMapping :: Map (Type, Type) Int
scoreMapping = M.fromList [((Rock,Paper), 8), ((Rock,Rock), 4), ((Rock,Scissors), 3), ((Paper,Paper), 5), ((Paper,Rock), 1), ((Paper,Scissors), 9), ((Scissors,Scissors), 6), ((Scissors,Rock), 7), ((Scissors,Paper), 2)]

partTwoTotalScore :: [[Type]] -> Int
partTwoTotalScore = sum . fmap partTwoRoundScore


partTwoRoundScore :: [Type] -> Int
partTwoRoundScore xs = case xs of Rock:Rock:xs -> roundScore $ Rock:Scissors:xs 
                                  Rock:Paper:xs -> roundScore $ Rock:Rock:xs
                                  Rock:Scissors:xs -> roundScore $ Rock:Paper:xs
                                  Paper:Rock:xs -> roundScore $ Paper:Rock:xs
                                  Paper:Paper:xs -> roundScore $ Paper:Paper:xs
                                  Paper:Scissors:xs -> roundScore $ Paper:Scissors:xs
                                  Scissors:Rock:xs -> roundScore $ Scissors:Paper:xs
                                  Scissors:Paper:xs -> roundScore $ Scissors:Scissors:xs
                                  Scissors:Scissors:xs -> roundScore $ Scissors:Rock:xs
                                  _ -> 0

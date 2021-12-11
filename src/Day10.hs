module Day10 where

import System.Environment   
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Stack
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe


solveBoth :: IO ()
solveBoth = do
    contents <- lines <$> readFile "./infiles/Day10.in"
    print $ solveA contents
    print $ solveB contents

starts :: Set Char 
starts = Set.fromList ['(', '[', '{', '<']

start2Close :: Map Char Char
start2Close = Map.fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

err2Score :: Map Char Int
err2Score = Map.fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

solveA :: [String] -> Int
solveA = sum . mapMaybe (corruptedCheck stackNew)

--Nothing means the line is good, Just x is the score of the syntax error
corruptedCheck :: Stack Char -> String -> Maybe Int
corruptedCheck stack [] = Nothing 
corruptedCheck stack (x:xs) 
  | Set.member x starts = corruptedCheck (stackPush stack x) xs
  | isJust check = corruptedCheck (fromJust check) xs
  | otherwise = Map.lookup x err2Score 
  where check = stackPop stack >>= \(st, y) -> 
              Map.lookup y start2Close >>= \c -> 
                  if x == c then Just st else Nothing

solveB :: [String] -> Int
solveB = (\xs -> xs !! (length xs `div` 2)) . sort . filter (1<) . map (completeLine stackNew) . discardCorrupted

discardCorrupted :: [String] -> [String]
discardCorrupted = mapMaybe (\xs -> if isJust (corruptedCheck stackNew xs) 
                                       then Nothing else Just xs)

stackToList :: Stack a -> [a]
stackToList stack = case stackPop stack of Just (st, x) -> x : stackToList st
                                           Nothing      -> []
char2Score :: Map Char Int
char2Score = Map.fromList [('(', 1), ('[', 2), ('{', 3), ('<', 4)]

--assumes non- corrupted string 
completeLine :: Stack Char -> String -> Int
completeLine stack [] = foldl (\acc x -> x + (acc*5)) 0 $ mapMaybe (`Map.lookup` char2Score) $ stackToList stack
completeLine stack (x:xs) 
  | Set.member x starts = completeLine (stackPush stack x) xs
  | otherwise = completeLine (fromJust check) xs
  where check = stackPop stack >>= \(st, y) -> 
              Map.lookup y start2Close >>= \c -> 
                  if x == c then Just st else Nothing




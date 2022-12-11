module AoC2022.Day5 where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec as Parsec
import Data.Stack
import Data.Char (isNumber, isLetter)
import Data.List (find, findIndex, transpose, zipWith)
import Data.Maybe (isJust,fromMaybe)
import Control.Monad

--n, from, to
type Instructions = [(Int, Int, Int)]

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day5.in"
    print $ reArrange $ parseInput fileContents
    print $ reArrangeLists $ parseInputLists fileContents


parseInput :: String -> (Map Int (Stack Char), Instructions)
parseInput inp = (stacks, instrs) 
    where stacks = Map.fromList $ zip [1..] $ map (foldl stackPush stackNew) stackSpecs
          stackSpecs = filter (not . null) $ map (filter isLetter) $ transpose $ reverse stackSpecLines
          maxStackSize = maybe 0 (1+) $ findIndex (isJust . find isNumber) $ lines inp
          stackSpecLines = take maxStackSize $ lines inp
          instrs = map ((\xs -> (read $ xs !! 1, read $ xs !! 3, read $ xs !! 5)) . words) instrLines
          instrLines = dropWhile ((['m']/=) . take 1) $ lines inp

reArrange :: (Map Int (Stack Char), Instructions) -> Map Int (Stack Char)
reArrange (stacks, []) = stacks
reArrange (stacks, x:xs) = reArrange (doInstruction stacks x, xs)

doInstruction :: Map Int (Stack Char) -> (Int, Int, Int) -> Map Int (Stack Char)
doInstruction stacks (0, from, to) = stacks
doInstruction stacks (n, from, to) = doInstruction (fromMaybe stacks newStacks) (n-1, from, to)
    where newStacks = do
            fromStack <- Map.lookup from stacks
            (newFromStack, elemToMove) <- stackPop fromStack
            return $ Map.adjust (`stackPush` elemToMove) to $ Map.insert from newFromStack stacks
       
--TODO: move the crates in the same order as picked up
parseInputLists :: String -> (Map Int [Char], Instructions)
parseInputLists inp = (lists, instrs) 
    where lists = Map.fromList $ zip [1..] listOfLists
          listOfLists = filter (not . null) $ map (filter isLetter) $ transpose  stackSpecLines
          maxStackSize = maybe 0 (1+) $ findIndex (isJust . find isNumber) $ lines inp
          stackSpecLines = take maxStackSize $ lines inp
          instrs = map ((\xs -> (read $ xs !! 1, read $ xs !! 3, read $ xs !! 5)) . words) instrLines
          instrLines = dropWhile ((['m']/=) . take 1) $ lines inp

reArrangeLists :: (Map Int [Char], Instructions) -> Map Int [Char]
reArrangeLists (lists, []) = lists
reArrangeLists (lists, x:xs) = reArrangeLists (doInstructionLists lists x, xs)

doInstructionLists :: Map Int [Char] -> (Int, Int, Int) -> Map Int [Char]
doInstructionLists lists (n, from, to) = fromMaybe lists newLists
    where newLists = do
            fromList <- Map.lookup from lists
            let elemsToMove = take n fromList
            return $ Map.adjust (elemsToMove ++) to $ Map.adjust (drop n) from lists

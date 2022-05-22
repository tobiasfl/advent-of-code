module AoC2021.Day8 where

import System.Environment   
import Data.List
import Data.List.Split(splitOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative
import Data.Maybe
import Data.Tuple

example = Map.fromList [('1',"ab"),('4',"eafb"),('7',"dab"),('8',"acedgfb")]

solveBoth :: IO ()
solveBoth = do
    contents <- lines <$> readFile "./infiles/Day8.in"
    --contents <- lines <$> readFile "./infiles/Day8Test.in"
    print $ solveA $ lines2patterns contents
    print $ solveB $ lines2patterns contents

lines2patterns :: [String] -> [([String], [String])]
lines2patterns = map (\xs -> (map sort $ head $ splitted xs, map sort $ last $ splitted xs))
    where splitted = map words . splitOn "|"

solveA :: [([String], [String])] -> Int
solveA = sum . map (countUniqDigs . snd)

countUniqDigs :: [String] -> Int
countUniqDigs = foldr (\xs -> if length xs `elem` [2,3,4,7] then (+1) else (+0)) 0

solveB :: [([String], [String])] -> Int
solveB = sum . map decode

decode :: ([String], [String]) -> Int
decode (inPattern, out) = read $ mapMaybe (\x -> Map.lookup x (decodeAll  (decodeSimple inPattern) inPattern)) out

decodeSimple :: [String] -> Map Char String
decodeSimple = Map.fromList . mapMaybe decodeSimpleElem

decodeSimpleElem :: String -> Maybe (Char, String)
decodeSimpleElem x 
  | length x == 2 = Just ('1', x)
  | length x == 4 = Just ('4', x)
  | length x == 3 = Just ('7', x)
  | length x == 7 = Just ('8', x)
  | otherwise = Nothing

decodeAll :: Map Char String -> [String] -> Map String Char
decodeAll m = Map.fromList . mapMaybe (decodeAllElem m)

decodeAllElem :: Map Char String -> String -> Maybe (String, Char)
decodeAllElem m x
  | length x == 6 && fromMaybe False (check x '7' 3) && not (fromMaybe False (check x '4' 4)) = Just (x, '0')
  | length x == 5 && fromMaybe False (check x '4' 2) = Just (x, '2')
  | length x == 5 && fromMaybe False (check x '7' 3) = Just (x, '3')
  | length x == 5 && fromMaybe False (check x '4' 3) = Just (x, '5')
  | length x == 6 && fromMaybe False (check x '4' 3) = Just (x, '6')
  | length x == 6 && fromMaybe False (check x '4' 4) = Just (x, '9')
  | otherwise = swap <$> decodeSimpleElem x
  where check x' y l = liftA2 (==) (length . intersect x' <$> Map.lookup y m) (Just l)

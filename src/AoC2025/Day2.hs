module AoC2025.Day2 where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.List
import Data.List.Extra
import Debug.Trace (trace)
import qualified Text.Parsec as Parsec
import Control.Applicative (Applicative(liftA2))

example :: String
example = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2025/Day2.in"
    let idRanges = parse fileContents
    print $ sum $ invalidIds idRanges
    print $ sum $ invalidIds2 idRanges
    pure ()

parse :: String -> [(Int, Int)]
parse = mapMaybe (parseTuple . splitOn "-") . splitOn ","
    where parseTuple [start, stop] = liftA2 (,) (readMaybe start) (readMaybe stop)
          parseTuple _ = Nothing

invalidIds :: [(Int, Int)] -> [Int]
invalidIds = filter (isInvalidId . show) . evenLengthNums . expand
    where expand = concatMap (\(s, e) -> [s..e])
          evenLengthNums = filter (even . length . show)
          isInvalidId idStr = uncurry (==) $ splitAt (length idStr `div` 2) idStr

invalidIds2 :: [(Int, Int)] -> [Int]
invalidIds2 = filter (isInvalidId . show) . expand
    where expand = concatMap (\(s, e) -> [s..e])

isInvalidId :: String -> Bool
isInvalidId num = any (\seqLen -> allSame $ chunksOf seqLen num) [1..(length num `div` 2)]

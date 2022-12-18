{-# LANGUAGE TupleSections #-}

module AoC2022.Day9 where

import Text.Read (readMaybe)
import Data.List (nub, mapAccumL)
import Debug.Trace (trace)

type Pos = (Int, Int)

type Step = (String, Int)

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day9.in"
    inp <- maybe (error "invalid input") pure $ parseInput fileContents
    print $ tailPosCountN 2 inp
    print $ tailPosCountN 10 inp

parseInput :: String -> Maybe [Step]
parseInput = traverse (parseLine . words) . lines
    where parseLine [dir, n] = (dir,) <$> readMaybe n
          parseLine _        = Nothing

tailPosCountN :: Int -> [Step] -> Int
tailPosCountN n = length . nub . map (drop (n-1)) . allMoves (replicate n (0, 0))

allMoves :: [Pos] -> [Step] -> [[Pos]]
allMoves ((hx, hy):knots) ((dir, 0):xs) = allMoves ((hx, hy):knots) xs
allMoves ((hx, hy):knots) ((dir, n):xs) = newPos:allMoves newPos ((dir, n-1):xs)
    where newPos = newTailPos (newHeadPos:knots)
          newHeadPos = case dir of
                     "R" -> (hx+1, hy)
                     "L" -> (hx-1, hy)
                     "U" -> (hx, hy+1)
                     "D" -> (hx, hy-1)
                     _   -> (hx, hy)
allMoves _ _ = []

newTailPos :: [Pos] -> [Pos]
newTailPos (hp@(hx, hy):tp@(tx, ty):knots)
  | onDiag && twoOff = hp:newTailPos ((horMove, vertMove):knots)
  | hx == tx && twoOff = hp:newTailPos ((tx, vertMove):knots)
  | hy == ty && twoOff = hp:newTailPos ((horMove, ty):knots)
  | otherwise = hp:newTailPos (tp:knots)
      where diff x y = max x y - min x y
            twoOff = diff hx tx >= 2 || diff hy ty >= 2
            onDiag = hx /= tx && hy /= ty
            vertMove = if hy > ty then ty+1 else ty-1
            horMove = if hx > tx then tx+1 else tx-1
newTailPos xs = xs

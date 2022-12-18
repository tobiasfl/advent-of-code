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
    print $ tailPosCount inp

parseInput :: String -> Maybe [Step]
parseInput = traverse (parseLine . words) . lines
    where parseLine [dir, n] = (dir,) <$> readMaybe n
          parseLine _        = Nothing

tailPosCount :: [Step] -> Int
tailPosCount = length . nub . map snd . allMoves ((0, 0), (0, 0))

allMoves :: (Pos, Pos) -> [Step] -> [(Pos, Pos)]
allMoves pos [] = [pos]
allMoves ((hx, hy), t) ((dir, 0):xs) = allMoves ((hx, hy), t) xs
allMoves ((hx, hy), t) ((dir, n):xs) = newPos:allMoves newPos ((dir, n-1):xs)
    where newPos = newTailPos (newHeadPos, t)
          newHeadPos = case dir of
                     "R" -> (hx+1, hy)
                     "L" -> (hx-1, hy)
                     "U" -> (hx, hy+1)
                     "D" -> (hx, hy-1)
                     _   -> (hx, hy)

newTailPos :: (Pos, Pos) -> (Pos, Pos)
newTailPos (hp@(hx, hy), tp@(tx, ty))
  | onDiag && twoOff = (hp, (horMove, vertMove))
  | hx == tx && twoOff = (hp, (tx, vertMove))
  | hy == ty && twoOff = (hp, (horMove, ty))
  | otherwise = (hp, tp)
      where diff x y = max x y - min x y
            twoOff = diff hx tx >= 2 || diff hy ty >= 2
            onDiag = hx /= tx && hy /= ty
            vertMove = if hy > ty then ty+1 else ty-1
            horMove = if hx > tx then tx+1 else tx-1




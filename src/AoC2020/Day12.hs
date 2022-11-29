module AoC2020.Day12 where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as M

data Instr = N Int | S Int | E Int | W Int | L Int | R Int | F Int
    deriving (Eq, Show)

data ShipDir = North | East | South | West
    deriving (Eq, Show)

data ShipPos = ShipPos
    { north :: Int
    , east :: Int
    , dir :: ShipDir}
    deriving (Eq, Show)

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2020/Day12.in"
    print $ findManhattanDist $ parseInstructions fileContents
--TODO: part B
parseInstructions :: String -> [Instr]
parseInstructions = mapMaybe parseInstr . lines
    where parseInstr [] = Nothing
          parseInstr (x:xs) = M.lookup x instrMapping <*> readMaybe xs
          instrMapping = M.fromList $ zip ['N','S','E','W','L','R','F'] [N,S,E,W,L,R,F]

startPos :: ShipPos
startPos = ShipPos 0 0 East

findManhattanDist :: [Instr] -> Int
findManhattanDist xs = if sumOfEastNorth < 0 then negate sumOfEastNorth else sumOfEastNorth
    where endPos = foldl execInstr startPos xs
          sumOfEastNorth = north endPos + east endPos

execInstr :: ShipPos -> Instr -> ShipPos
execInstr p@(ShipPos n e d) i = case i of N x -> ShipPos (n+x) e d
                                          S x -> ShipPos (n-x) e d
                                          E x -> ShipPos n (e+x) d
                                          W x -> ShipPos n (e-x) d
                                          F x -> goForward p x
                                          _ -> ShipPos n e (rotate d i)

goForward :: ShipPos -> Int -> ShipPos
goForward (ShipPos n e d) x = case d of North -> ShipPos (n+x) e d
                                        East -> ShipPos n (e+x) d
                                        South -> ShipPos (n-x) e d
                                        West -> ShipPos n (e-x) d

rotate :: ShipDir -> Instr -> ShipDir
rotate d i = case i of L x -> leftDirs d !! index x
                       R x -> rightDirs d !! index x
                       _  -> d
               where rightDirs currDir = dropWhile (/= currDir) $ cycle allDirs
                     leftDirs currDir = dropWhile (/= currDir) $ cycle $ reverse allDirs
                     allDirs = [North, East, South, West]
                     index degreesToRotate = max 0 $ degreesToRotate `div` 90

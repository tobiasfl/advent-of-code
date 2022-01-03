module Day17 where

import System.Environment   
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Text.Parsec as Parsec
import Data.Either

solveBoth :: IO ()
solveBoth = do
    input <- readFile "./infiles/Day17.in"
    print $ solveA $ parseInput input

-- have a set with possible finishing coordinates
parseInput :: String -> ((Int, Int), (Int, Int))
parseInput = fromRight ((0,0),(0,0)) . parse coordParser ""
    where coordParser = do
            manyTill anyChar (char '=')
            xS <- manyTill anyChar (string "..")
            xE <- manyTill anyChar (char ',')
            manyTill anyChar (char '=')
            yS <- manyTill anyChar (string "..")
            yE <- manyTill anyChar (char '\n')
            return ((read xS, read xE), (read yS, read yE))

type Velocity = (Int, Int)
type Coordinate = (Int, Int)

solveA :: ((Int, Int), (Int, Int)) -> Int
solveA ((_, _), (yS, _)) = n * (n + 1) `div` 2
    where n = negate yS - 1

--goodPaths :: ((Int, Int), (Int, Int)) -> Map Velocity [Coordinate]
--goodPaths goal = M.filter (isGoodPath goal) $ doNSteps 500 goal

--do steps and returns paths that pass by the goal area
--TODO: just run until they have gone lower than the goal area
--doNSteps :: Int -> ((Int, Int), (Int, Int)) -> Map Velocity [Coordinate]
--doNSteps n = M.mapWithKey (\velocity startCoord -> take n $ iterate (step velocity) startCoord) . allVels

isGoodPath :: ((Int, Int), (Int, Int)) -> Velocity -> Bool
isGoodPath ((xS, xE), (yS, yE)) v = undefined


--create each path, limited to a min and max velocity
allVels :: ((Int, Int), (Int, Int)) -> [Velocity]
allVels ((xS, xE), (yS, yE)) = [(x, y) | x <- [xMin..xMax], y <- [yMin..yMax]]
    where xMax = max 0 (max xS xE)
          xMin = min 0 (min xS xE)
          yMin = min 0 (min yS yE)
          yMax = 300 --TODO: just an arbitryary number, don't know how to find the optimal value. 
              --TODO: could a minimum y veloecity be determined?

step :: Velocity -> Coordinate -> Coordinate
step (xVel, yVel) (x, y)  =  (xPos, y + yVel - 1)
    where xPos 
            | xVel > 0 = if (x + xVel) - 1 <= 0 then 0 else (x + xVel) - 1
            | xVel < 0 = if (x + xVel) + 1 >= 0 then 0 else (x + xVel) + 1
            | otherwise = 0

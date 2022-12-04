module AoC2020.Day12 where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as M

--move a waypoint relativ to the ships pos

--Action N means to move the waypoint north by the given value.
--Action S means to move the waypoint south by the given value.
--Action E means to move the waypoint east by the given value.
--Action W means to move the waypoint west by the given value.
--Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
--Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
--Action F means to move forward to the waypoint a number of times equal to the given value.


data Instr = N Int | S Int | E Int | W Int | L Int | R Int | F Int
    deriving (Eq, Show)

data ShipDir = North | East | South | West
    deriving (Eq, Show)

data ShipPos = ShipPos
    { north :: Int
    , east :: Int
    , dir :: ShipDir}
    deriving (Eq, Show)

data XYPos = XYPos
    { wpNorth :: Int
    , wpEast :: Int }
    deriving (Eq, Show)

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2020/Day12.in"
    print $ findManhattanDist $ parseInstructions fileContents

parseInstructions :: String -> [Instr]
parseInstructions = mapMaybe parseInstr . lines
    where parseInstr [] = Nothing
          parseInstr (x:xs) = M.lookup x instrMapping <*> readMaybe xs
          instrMapping = M.fromList $ zip ['N','S','E','W','L','R','F'] [N,S,E,W,L,R,F]

shipStartPos :: ShipPos
shipStartPos = ShipPos 0 0 East

findManhattanDist :: [Instr] -> Int
findManhattanDist xs = if sumOfEastNorth < 0 then negate sumOfEastNorth else sumOfEastNorth
    where endPos = foldl execInstr shipStartPos xs
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

waypointStartPos :: XYPos
waypointStartPos = XYPos 1 10 

--TODO: This would be easier if you used waypoint coords only in x,y relation to the ship instead..

--execWaypointInstr :: (XYPos, XYPos) -> Instr -> (XYPos, XYPos)
--execWaypointInstr (sp@(XYPos n e), wp@(XYPos wpN wpE)) i =
--    case i of N x -> (sp, XYPos (wpN+x) wpE) 
--              S x -> (sp, XYPos (wpN-x) wpE)
--              E x -> (sp, XYPos wpN (wpE+x))
--              W x -> (sp, XYPos wpN (wpE-x))
--              F x -> (XYPos ((wpN - n) * x) ((wpE - e) * x), wp)
--              _   -> rotateWaypoint (sp, wp) i
--
--rotateWaypoint :: (XYPos, XYPos) -> Instr -> (XYPos, XYPos)
--rotateWaypoint (sp@(XYPos n e), wp@(XYPos wpN wpE)) i =
--    case i of L 180 -> (sp, (XYPos (turn n wpN) (turn e wpE)))
--              R 180 -> (sp, (XYPos (turn n wpN) (turn e wpE)))
--              L 90 -> undefined
--              R 90 -> undefined
--              L 270 -> undefined
--              R 270 -> undefined
--              _   -> ((XYPos n e), (XYPos wpN wpE))
--                  where turn shipCoord wpointCoord = shipCoord + (shipCoord - wpointCoord)

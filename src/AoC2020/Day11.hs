module AoC2020.Day11 where

import Text.Read (readMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Multimap (Multimap)
import qualified Data.Multimap as MM
import Data.List (mapAccumL, group, find)
import Data.Maybe (mapMaybe,fromJust,fromMaybe, catMaybes)
import Debug.Trace (trace)

data Space = Empty | Occupied | Floor
    deriving (Eq, Show)

type Grid = Map (Int, Int) Space

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2020/Day11.in"
    let grid = parseGrid fileContents
    print $ stableRoundOccupiedSeats grid
    print $ stableRoundOccupiedSeats' grid

parseGrid :: String -> Grid
parseGrid = M.fromList . addCoords . parseChars . lines
    where parseChars = fmap (fmap parseChar)
          addCoords = concat . zipWith (\xC -> zipWith (\yC v -> ((xC, yC), v)) [0..]) [0..]
          parseChar c = case c of 'L' -> Empty
                                  '#' -> Occupied
                                  _   -> Floor

stableRoundOccupiedSeats :: Grid -> Int
stableRoundOccupiedSeats = length . M.filter (Occupied==) . stableStateRound

stableStateRound :: Grid -> Grid
stableStateRound g = if runRound g == g then g else stableStateRound (runRound g)

runRound :: Grid -> Grid
runRound g = M.mapWithKey (\k v -> updateSpace k g) g

updateSpace :: (Int, Int) -> Grid -> Space
updateSpace k g = newState $ M.lookup k g
    where newState s = case s of Just Occupied -> if occupiedCount >= 4 
                                                     then Empty 
                                                     else Occupied
                                 Just Empty    -> if occupiedCount == 0 
                                                     then Occupied 
                                                     else Empty
                                 _             -> Floor
          occupiedCount = length $ filter (Occupied ==) (adjacentSpaces k g)

adjacentSpaces :: (Int, Int) -> Grid -> [Space]
adjacentSpaces (x, y) g = mapMaybe (`M.lookup` g) adjacentCoords
    where adjacentCoords = filter ((x, y) /=) [(ax, ay) | ax <- [x-1, x, x+1], ay <- [y-1, y, y+1]]

stableRoundOccupiedSeats' :: Grid -> Int
stableRoundOccupiedSeats' g = length $ M.filter (Occupied==) $ stableStateRound' (spacesInView g) g

stableStateRound' :: Multimap (Int, Int) (Int, Int) -> Grid -> Grid
stableStateRound' mm g = if afterRound == g then g else stableStateRound' mm afterRound
    where afterRound = runRound' mm g

runRound' :: Multimap (Int, Int) (Int, Int) -> Grid -> Grid
runRound' mm g = M.mapWithKey (\k v -> updateSpace' mm k g) g

spacesInView :: Grid -> Multimap (Int, Int) (Int, Int)
spacesInView g = MM.fromList $ do
        (p, v) <- M.toList g
        let dirs = [(0, 1), (0, -1), (1, 0), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
        zip (repeat p) (mapMaybe (spaceInDir p) dirs)
            where spaceInDir (xp, yp) d@(xd, yd) = let newPos = (xp+xd, yp+yd) in
                    case M.lookup newPos g of Just Floor -> spaceInDir newPos d
                                              Just _     -> Just newPos
                                              Nothing    -> Nothing

updateSpace' :: Multimap (Int, Int) (Int, Int) -> (Int, Int) -> Grid -> Space
updateSpace' mm k g = newState $ M.lookup k g
    where occupiedCount = length $ filter (Occupied ==) $ mapMaybe (`M.lookup` g) $ MM.lookup k mm
          newState s = case s of Just Occupied -> if occupiedCount >= 5
                                                              then Empty
                                                              else Occupied
                                 Just Empty    -> if occupiedCount == 0
                                                              then Occupied
                                                              else Empty
                                 _             -> Floor

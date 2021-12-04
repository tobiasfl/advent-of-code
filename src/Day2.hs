module Day2 where

import System.Environment   
import Data.List  

solveBoth :: IO ()
solveBoth = do
    (filename:args) <- getArgs
    contents <- readFile filename 
    print $ toTuples contents
    print $ solveA $ toTuples contents 
    print $ solveB $ toTuples contents
  
toTuples :: String -> [(String, Int)]
toTuples = map ((\xs -> (head xs, read $ last xs)) . words) . lines

--Part One
solveA :: [(String, Int)] -> Int
solveA = uncurry (*) .  foldl cmdToPos (0, 0) 

cmdToPos ::  (Int, Int) -> (String, Int) -> (Int, Int)
cmdToPos (d, h) (cmd, n) = case cmd of "forward" -> (d, h+n)
                                       "down"    -> (d+n, h)
                                       "up"      -> (d-n, h)
                                       _         -> (d, h)

--Part Two
solveB :: [(String, Int)] -> Int
solveB = (\(d, h, a) -> d * h) . foldl cmdToPos' (0, 0, 0) 

cmdToPos' ::(Int, Int, Int) -> (String, Int) ->  (Int, Int, Int)
cmdToPos' (d, h, a) (cmd, n)  = case cmd of "forward" -> (d+(a*n), h+n, a)
                                            "down"    -> (d, h, a+n)
                                            "up"      -> (d, h, a-n)
                                            _         -> (d, h, a)


module AoC2024.Day4 where

import Debug.Trace (trace)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC
import qualified Text.Parsec.Combinator as PCOMB
import Data.List (transpose)
import Text.Read (readMaybe)
import Data.Either (fromRight)
import Data.Maybe (catMaybes, isJust)

data Instruction = Mul Int Int
    deriving (Show)

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2024/Day4.in"
    print fileContents

--expandToAllDirs :: [String] -> [String]
--expandToAllDirs hor = forwardDirs ++ fmap reverse forwardDirs
--    where forwardDirs = concat [hor, vert, diagFromLeft, diagFromRight]
--          vert = transpose hor
--          diagFromLeft = takeWhile isJust $ fmap (\startC -> zipWith (\r c -> (hor !? r) >>= (!? c)) [0..] [startC..]) [0..]
--          diagFromRight = undefined
--


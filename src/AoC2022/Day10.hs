{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module AoC2022.Day10 where

import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

--x register and cycle num
type State = (Int, Int)

data Instr = AddX Int | Noop
    deriving (Show, Eq)

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day10.in"
    print $ findRelevantStates $ parse fileContents
    print $ sumOfSigStrengths $ parse fileContents
    putStr $ statesToString $ parse fileContents

parse :: String -> [Instr]
parse = let parseLine x = case x of ["addx", xs] -> AddX <$> readMaybe xs
                                    ["noop"]     -> Just Noop
                                    _            -> Nothing
         in mapMaybe (parseLine . words) . lines

startState :: State
startState = (1, 0)

--A single register X starts at the value 1
--addx V takes two cycles to complete, afterwards the X register is increased/decreased by V
--noop take one cycle to complete, no effect

--goal: cycle number multiplied by the x register, during the 20th cycle and every 40 cycles after that

sumOfSigStrengths :: [Instr] -> Int
sumOfSigStrengths = sum . map (uncurry (*)) . findRelevantStates

findRelevantStates :: [Instr] -> [State]
findRelevantStates = filter (\(x, cycle) -> cycle `elem` [20, 60, 100, 140, 180, 220]) . cycleStates

cycleStates :: [Instr] -> [State]
cycleStates = concat . scanl (execInstr . last) [startState]

execInstr :: State -> Instr -> [State]
execInstr (x, cycleNum) Noop = [(x, cycleNum+1)]
execInstr (x, cycleNum) (AddX n) = [(x, cycleNum+1), (x+n, cycleNum+2)]

--CRT width: 40 height 6
--sprite is 3px wide
--If the sprite is positioned such that one of its three pixels is the pixel currently being drawn
--The CRT draws a single pixel during each cycle
--lit pixel: #
--dark: .

statesToString :: [Instr] -> String
statesToString = withNewlines . asChars . cycleStates
    where asChars = map (\(x, c) -> let cycle = c `mod` 40 in if x `elem` [cycle-1, cycle, cycle+1] then '#' else '.')
          withNewlines [] = []
          withNewlines xs = take 40 xs ++ "\n" ++ withNewlines (drop 40 xs)



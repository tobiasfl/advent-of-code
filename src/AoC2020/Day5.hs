module AoC2020.Day5 where

import Data.Maybe
import qualified Data.Set as S

--rows:0-127
--F -> lower half of row range
--B -> Upper half of row range
--columns:0-7
--L -> lower half of col range
--R -> upper half of col range
--Seat ID: (row * 8) + col

--lower row, upper row, lower col, upper col
type Coord = (Int, Int, Int, Int)

data SeatChar = F | B | L | R deriving Show

parseLine :: String -> [SeatChar]
parseLine = mapMaybe mapper
    where mapper c = case c of 'F' -> Just F
                               'B' -> Just B
                               'L' -> Just L
                               'R' -> Just R
                               _   -> Nothing

solveBoth :: IO ()
solveBoth = do
    allInpRows <- map parseLine . lines <$> readFile "./infiles/AoC2020/Day5.in"
    print $ solveA allInpRows
    print $ solveB allInpRows

solveA :: [[SeatChar]] -> Int
solveA = maximum . map rowSeatId

solveB :: [[SeatChar]] -> Int
solveB xs = head $ dropWhile (`S.member` seatIds) [front..back]
    where front = minimum seatIds
          back = maximum seatIds
          seatIds = S.fromList $ map rowSeatId xs

rowSeatId :: [SeatChar] -> Int
rowSeatId = (\(lR, uR, lC, uC) -> (uR*8) + uC) . row

row :: [SeatChar] -> Coord
row = foldl step (0, 127, 0, 7)

step :: Coord -> SeatChar -> Coord
step (lR, uR, lC, uC) c = case c of F -> (lR, uR - halfDiff lR uR, lC, uC)
                                    B -> (lR + halfDiff lR uR, uR, lC, uC)
                                    L -> (lR, uR, lC, uC - halfDiff lC uC)
                                    R -> (lR, uR, lC + halfDiff lC uC, uC)
    where halfDiff l u = 1+(u-l) `div` 2

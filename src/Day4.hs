module Day4 where

import System.Environment   
import Data.List  
import Data.List.Split
import Data.Maybe(isNothing)

solveBoth :: IO ()
solveBoth = do
    realContents <- readFile "./infiles/Day4.txt"
    print $ solveA $ lines realContents
    print $ solveB $ lines realContents
    

data Square = Unmarked Int | Marked Int deriving (Show, Eq)

squareToInt :: Square -> Int
squareToInt (Unmarked x) = x
squareToInt (Marked x) = x

squareIsMarked :: Square -> Bool
squareIsMarked (Marked _)   = True
squareIsMarked (Unmarked _) = False

type Board = [[Square]] 

mkBoard :: [String] -> Board
mkBoard = map (map (Unmarked . read) . words)

parseNums :: String -> [Int]
parseNums = map read . splitOneOf "," 

parseBoards :: [String] -> [Board]
parseBoards = map mkBoard . filter (not . null) . splitWhen null 

solveA :: [String] -> Int
solveA strLines = fstWinScore (parseNums (head strLines)) (parseBoards $ tail strLines)

fstWinScore :: [Int] -> [Board] -> Int
fstWinScore [] _ = 0
fstWinScore (n:ns) bs = case winner of Nothing -> fstWinScore ns (playRound n bs) 
                                       Just b -> sumOfUnmarked b * n
    where winner = find checkBoard (playRound n bs)

playRound :: Int -> [Board] -> [Board]
playRound n = map (markBoard n)  

sumOfUnmarked :: Board -> Int
sumOfUnmarked = sum . map (sum . map squareToInt . filter (not . squareIsMarked)) 

checkBoard :: Board -> Bool
checkBoard b = any lineIsMarked b || any lineIsMarked (transpose b) 
    where lineIsMarked = all squareIsMarked

markBoard :: Int -> Board -> Board
markBoard n = map (map (markIfEqual n))

markIfEqual :: Int -> Square -> Square
markIfEqual n (Unmarked x) = if n == x then Marked x else Unmarked x
markIfEqual _ (Marked x)   = Marked x

solveB :: [String] -> Int
solveB strLines = lastWinScore (parseNums (head strLines)) (parseBoards $ tail strLines)

lastWinScore :: [Int] -> [Board] -> Int
lastWinScore [] [] = 0
lastWinScore [] (b:bs) = sumOfUnmarked b
lastWinScore (n:ns) bs = if all checkBoard roundRes
                          then sumOfUnmarked (last roundRes) * n
                          else lastWinScore ns (remWinners roundRes) 
                              where remWinners = filter (not . checkBoard) 
                                    roundRes = playRound n bs

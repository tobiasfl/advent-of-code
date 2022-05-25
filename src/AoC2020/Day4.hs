module AoC2020.Day3 where

solveBoth :: IO ()
solveBoth = do
    allLines <- lines <$> readFile "./infiles/AoC2020/Day4.in"
    print allLines

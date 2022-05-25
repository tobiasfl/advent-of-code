module AoC2020.Day1 where


import System.Environment   

solveA :: IO ()
solveA = do
    nums <- map read . lines <$> readFile "./infiles/AoC2020/Day1.in" :: IO [Int]
    print $ bruteForceA 2020 nums
    print $ bruteForceB 2020 nums

bruteForceA :: Int -> [Int] -> Int
bruteForceA target xs = head $ do
    x <- xs
    y <- xs
    [x*y | x+y == target]

bruteForceB :: Int -> [Int] -> Int
bruteForceB target xs = head $ do
    x <- xs
    y <- xs
    z <- xs
    [x*y*z | x+y+z == target]


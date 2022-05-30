module AoC2020.Day4 where

import Data.List.Split(splitOn,splitOneOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Text.Read
import Data.List
import Data.Char
import Data.Ix

type Field = (String, String)

solveBoth :: IO ()
solveBoth = do
    allInput <- readFile "./infiles/AoC2020/Day4.in"
    print $ length $ filter isValidPassportA $ parsePassports allInput
    print $ length $ filter isValidPassportB $ parsePassports allInput

parsePassports :: String -> [[Field]]
parsePassports = fmap parsePassport . splitOn "\n\n"
    where parsePassport = mapMaybe maybeParseField . splitOneOf " \n"
          maybeParseField = (\xs -> if length xs == 2 then Just (head xs, last xs) else Nothing) . splitOn ":"

isValidPassportA :: [Field] -> Bool
isValidPassportA xs
  | length xs == 8                                                = True
  | isNothing (find (\(k, v) -> k == "cid") xs) && length xs == 7 = True
  | otherwise                                                     = False

isValidPassportB :: [Field] -> Bool
isValidPassportB = all (\(f, n) -> n >= 1)  . foldr checkAll checks 
    where checks = [(f, 0) | f <- allVFuncs]
          checkAll field = map (\(f, n) -> if f field then (f, n+1) else (f, n)) 

allVFuncs :: [Field -> Bool]
allVFuncs = [validByr,validIyr,validEyr,validHgt,validHcl,validEcl,validPid]

checkYearVal :: (Int, Int) -> String -> Bool
checkYearVal (minV, maxV) v = fromMaybe False $ readMaybe v >>= \y -> return $ inRange (minV, maxV) y

validByr :: Field -> Bool
validByr (k, v) = k == "byr" && checkYearVal (1920, 2002) v

validIyr :: Field -> Bool
validIyr (k, v) = k == "iyr" && checkYearVal (2010, 2020) v

validEyr :: Field -> Bool
validEyr (k, v) = k == "eyr" && checkYearVal (2020, 2030) v

validHgt :: Field -> Bool
validHgt ("hgt", v) 
  | "cm" `isSuffixOf` v = fromMaybe False (asMaybeInt >>= \x -> return (inRange (150, 193) x))
  | "in" `isSuffixOf` v = fromMaybe False (asMaybeInt >>= \x -> return (inRange (59, 76) x))
  | otherwise = False
      where asMaybeInt = readMaybe $ takeWhile isDigit v
validHgt _ = False

validHcl :: Field -> Bool
validHcl ("hcl", '#':xs) = all (\x -> x `elem` ['0'..'9'] ++ ['a'..'f']) xs && length xs == 6
validHcl _ = False

validEcl :: Field -> Bool
validEcl (k, v) = k == "ecl" && v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPid :: Field -> Bool
validPid (k, v) = k == "pid" && all isDigit v && length v == 9



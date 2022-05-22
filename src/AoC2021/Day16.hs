module AoC2021.Day16 where

import Lib(intToBin, binToInt)
import System.Environment   
import Data.List
import Data.Char(digitToInt, isHexDigit)
import Data.Maybe
import Text.Parsec as Parsec
import Text.Parsec ((<|>))
import Control.Applicative
import Control.Monad.Identity (Identity)
import Control.Monad

type Header = (Int, Int)

data Packet = Literal Header Int
               | Operator Header [Packet] deriving (Show)

solveBoth :: IO ()
solveBoth = do
    input <- readFile "./infiles/Day16.in"
    print $ solveA input
    print $ solveB input

solveA :: String -> Either ParseError Int
solveA = either Left solve . parse binParser ""
    where solve = fmap sumOfVersions . parse parsePacket ""

sumOfVersions :: Packet -> Int
sumOfVersions (Operator (v, t) xs) = v + foldr (\x acc -> acc + sumOfVersions x) 0 xs
sumOfVersions (Literal (v, t) x) = v

solveB :: String -> Either ParseError Int
solveB = either Left solve . parse binParser ""
    where solve = fmap evalExpression . parse parsePacket "" 

evalExpression :: Packet -> Int
evalExpression (Literal h v) = v
evalExpression (Operator (v, t) xs) = case t of 0 -> sum $ map evalExpression xs
                                                1 -> product $ map evalExpression xs
                                                2 -> minimum $ map evalExpression xs
                                                3 -> maximum $ map evalExpression xs
                                                5 -> if evalExpression (head xs) > evalExpression (last xs) then 1 else 0
                                                6 -> if evalExpression (head xs) < evalExpression (last xs) then 1 else 0
                                                7 -> if evalExpression (head xs) == evalExpression (last xs) then 1 else 0
                                                _ -> -1

hexToBin :: Char -> Maybe String
hexToBin c = if isHexDigit c then Just $ padBin $ intToBin $ digitToInt c else Nothing
    where padBin xs =  replicate (4 - length xs) '0' ++ xs 

binParser :: Parsec.Parsec String () String
binParser = do
    hexes <- Parsec.many1 (Parsec.oneOf "0123456789ABCDEF")
    let asBins = if all (isJust . hexToBin) hexes then Just $ concat $ mapMaybe hexToBin hexes else Nothing
    maybe (fail "hexToBin returned Nothing") return asBins

parsePacket :: Parsec.Parsec String () Packet
parsePacket = do
    (version, typeId) <- parseHeader
    if typeId == 4 then parseLiteral (version, typeId) else parseOperator (version, typeId)
 
parseTrioInt :: Parsec.Parsec String () Int
parseTrioInt = binToInt <$> Parsec.count 3 (Parsec.oneOf "01")

parseHeader :: Parsec.Parsec String () Header
parseHeader = do
    version <- parseTrioInt
    typeId <- parseTrioInt
    return (version, typeId)

parseLiteral :: Header -> Parsec.Parsec String () Packet
parseLiteral header = do
    Literal header . binToInt <$> parseLiteralGroups

parseLiteralGroups :: Parsec.Parsec String () String
parseLiteralGroups = do
    prefix <- Parsec.oneOf "01"
    num <- Parsec.count 4 (Parsec.oneOf "01")
    if prefix == '0' then return num else fmap (num ++) parseLiteralGroups

parseOperator :: Header -> Parsec.Parsec String () Packet
parseOperator header = do
    subs <- parseOperatorLenSubs Parsec.<|> parseOperatorNumSubs
    return $ Operator header subs

parseOperatorLenSubs :: Parsec.Parsec String () [Packet]
parseOperatorLenSubs = do
    Parsec.char '0'
    subPacketsLen <- binToInt <$> Parsec.count 15 (Parsec.oneOf "01")
    parseSubPackets subPacketsLen

parseSubPackets :: Int -> Parsec.Parsec String () [Packet]
parseSubPackets n = do
    inputBefore <- getInput
    packet <- parsePacket 
    inputAfter <- getInput
    let lenParsed = length inputBefore - length inputAfter
    if lenParsed < n then (packet :) <$> parseSubPackets (n-lenParsed) else return [packet]

parseOperatorNumSubs :: Parsec.Parsec String () [Packet]
parseOperatorNumSubs = do
    Parsec.char '1'
    numSubs <- binToInt <$> Parsec.count 11 (Parsec.oneOf "01")
    Parsec.count numSubs (do
        parsePacket)

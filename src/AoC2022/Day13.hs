module AoC2022.Day13 where

import Text.Parsec as P
import Text.Read (readMaybe)
import Data.Maybe (maybe)
import Control.Applicative (Alternative(empty))
import Data.Either (fromRight)
import Data.List (sort, findIndices)
import Debug.Trace (trace)

data Packet = PInt Int | PList [Packet]
    deriving (Show, Eq)

instance Ord Packet where
    compare (PInt left) (PInt right) = compare left right
    compare (PInt left) (PList right) = compare (PList [PInt left]) (PList right)
    compare (PList left) (PInt right) = compare (PList left) (PList [PInt right])
    compare (PList left) (PList right) = compare left right

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day13.in"
    let packetPairs = fromRight [] $ parseInput fileContents
    print $ sumOfIndicesSortedPairs packetPairs
    print $ decoderKey $ concatMap (\(x, y) -> [x, y]) packetPairs

parseInput :: String -> Either ParseError [(Packet, Packet)]
parseInput = parse packetPairsParser ""

packetPairsParser :: P.Parsec String () [(Packet, Packet)]
packetPairsParser = P.many1 $ do
        p1 <- packetListParser
        P.newline
        p2 <- packetListParser
        P.newline
        P.optional P.newline
        return (p1, p2)

packetListParser :: P.Parsec String () Packet
packetListParser = do
    P.char '['
    packets <- P.many (packetIntParser <|> packetListParser )
    P.char ']'
    P.optional (P.char ',')
    return $ PList packets

packetIntParser :: P.Parsec String () Packet
packetIntParser = do
    digits <- P.many1 P.digit
    P.optional (P.char ',')
    maybe empty (return . PInt) $ readMaybe digits

sumOfIndicesSortedPairs :: [(Packet, Packet)] -> Int
sumOfIndicesSortedPairs = sum . map fst . filter (uncurry (<) . snd) . zip [1..]

dividerPackets :: [Packet]
dividerPackets = [PList [PList [PInt 2]], PList [PList [PInt 6]]]

decoderKey :: [Packet] -> Int
decoderKey = product . map (+1) . findIndices (`elem` dividerPackets) . sort . (dividerPackets++)

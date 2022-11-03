--{-# LANGUAGE TupleSections #-}

module AoC2020.Day7 where

import Text.Parsec as P
import Text.Parsec ((<|>))
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Multimap (Multimap)
import qualified Data.Multimap as MM
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

type Rules = Multimap Color Color

type Rule = (Color, Set Color)

data Color = Color String (Maybe Integer)
    deriving (Show, Ord, Eq)

--a.
--How many bag colors can eventually contain at least one shiny gold bag? 
--b.
--How many individual bags are required inside your single shiny gold bag?

solve :: IO ()
solve = do
    fileContents <- lines <$> readFile  "./infiles/AoC2020/Day7.in"
    rules <- parseRules fileContents
    print $ length (validColors (removeFromColorNums $ reverseRules rules))
    print $ bagsRequired rules

goal :: Color
goal = Color "shiny gold" Nothing

--a
validColors :: Rules -> [Color]
validColors rules = nub $ connectedBags rules $ MM.lookup goal rules

connectedBags :: Rules -> [Color] -> [Color]
connectedBags rules [] = []
connectedBags rules (x:xs) = x:connectedBags rules (MM.lookup x rules ++ xs)

reverseRules :: Rules -> Rules
reverseRules = MM.fromList . MM.foldrWithKey (\k v acc -> (v, k):acc) []

removeFromColorNums :: Rules -> Rules
removeFromColorNums = MM.fromList . MM.foldrWithKey (\(Color s n) v acc -> (Color s Nothing, v):acc) []

--b
bagsRequired :: Rules -> Integer
bagsRequired rules = sum $ map (bagCounts rules) (MM.lookup goal rules)

bagCounts :: Rules -> Color -> Integer
bagCounts rules (Color s n) = num + sum (map ((num*) . bagCounts rules) nextBags)
    where num = fromMaybe 1 n
          nextBags = MM.lookup (Color s Nothing) rules

--Parsing stuff
parseRules :: [String] -> IO Rules
parseRules xs = 
    let parseRes = traverse (P.parse ruleParser "") xs in
    case parseRes of Right rules -> return (MM.fromList $ concatMap (\(c, cs) -> [(c,x) | x<- S.toList cs]) rules)
                     Left err -> fail (show err)

ruleParser :: P.Parsec String () Rule
ruleParser = do
    fromColor <- fromColorParser
    toColors <- toColorsParser
    return (fromColor, S.fromList toColors)

fromColorParser :: P.Parsec String () Color
fromColorParser = Color 
                    <$> P.manyTill P.anyChar (P.try $ P.string " bags contain ") 
                    <*> pure Nothing

toColorsParser :: P.Parsec String () [Color]
toColorsParser = P.many $ do
                n <- digitToInt <$> P.digit
                P.space
                x <- P.many1 (P.oneOf ['a'..'z'])
                P.space
                y <- P.many1 (P.oneOf ['a'..'z'])
                P.string " bag"
                P.optional (P.char 's')
                P.string ", " <|> P.string "."
                return $ Color (x ++ " " ++ y) (Just $ toInteger n)

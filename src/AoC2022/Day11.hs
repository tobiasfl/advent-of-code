module AoC2022.Day11 where

import Data.Map (Map)
import qualified Data.Map as Map
import Text.Parsec as Parsec
import Text.Parsec ((<|>))
import Text.Read (readMaybe)
import Data.Maybe (isJust, fromMaybe)
import Control.Monad (guard)
import Data.Functor (($>))
import Data.List (sortBy)

type Monkeys = Map Int Monkey

--TODO: this one could be rewritteb with lenses
data Monkey = Monkey
    { items       :: [Int]
    , op          :: Int -> Int
    , test        :: Int -> Int  --result is id of monkey to throw to
    , inspections :: Int
    , divBy       :: Int}

addItem :: Int -> Monkey -> Monkey
addItem newItem (Monkey i op t insp db) = Monkey (i++[newItem]) op t insp db

instance Show Monkey where
    show (Monkey i o t insp db) = "Monkey items:" ++ show i ++ " inspections:" ++ show insp

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day11.in"
    print $ monkeyBusiness 21 <$> parse parseMonkeys "" fileContents
    print $ monkeyBusiness 10001 <$> parse parseMonkeys "" fileContents

parseMonkeys :: Parsec.Parsec String () Monkeys
parseMonkeys = do
    monkeys <- many1 parseMonkey
    return $ Map.fromList $ zip [0..] monkeys

parseMonkey :: Parsec.Parsec String () Monkey
parseMonkey = do
    string "Monkey " >> digit >> string ":" >> newline >> spaces
    string "Starting items: "
    items <- many parseItem
    operation <- parseOperation
    (db, test) <- parseMTest
    choice [newline, eof $> '\n']
    return $ Monkey items operation test 0 db

parseItem :: Parsec.Parsec String () Int
parseItem = do
    digits <- many1 digit
    string ", " <|> many1 newline
    maybe (fail "Failed to read item number") return $ readMaybe digits

parseOperation :: Parsec.Parsec String () (Int -> Int)
parseOperation = do
    spaces >> string "Operation: new = old "
    operator <- (\o -> if o == '+' then (+) else (*)) <$> oneOf "+*"
    space
    rightSide <- string "old" <|> many1 digit
    newline
    return (case readMaybe rightSide of Nothing -> (\old -> operator old old)
                                        Just x  -> (`operator` x))
parseMTest :: Parsec.Parsec String () (Int, Int -> Int)
parseMTest = do
    spaces >> string "Test: divisible by "
    divBy <- many1 digit >>= maybe (fail "Failed to read divBy") return . readMaybe
    newline >> spaces >> string "If true: throw to monkey "
    ifTrue <- many1 digit >>= maybe (fail "Failed to read ifTrue") return . readMaybe
    newline >> spaces >> string "If false: throw to monkey "
    ifFalse <- many1 digit >>= maybe (fail "Failed to read ifFalse") return . readMaybe
    newline
    return (divBy, \worryLevel -> if worryLevel `mod` divBy == 0 then ifTrue else ifFalse)

monkeyBusiness :: Int -> Monkeys -> Int
monkeyBusiness n = foldl (\acc m -> acc * inspections m) 1 . mostActive . doRounds n

mostActive :: Monkeys -> [Monkey]
mostActive = take 2 . sortBy (\m1 m2 -> compare (inspections m2) (inspections m1)) . Map.elems

doRounds :: Int -> Monkeys -> Monkeys
doRounds n monkeys = last $ take n $ iterate (doRound (Just supermod)) monkeys
    where supermod = foldl (\acc m2 -> acc * divBy m2) 1 (Map.elems monkeys)

doRound :: Maybe Int -> Monkeys -> Monkeys
doRound supermod monkeys = foldl (monkeyRound supermod) monkeys (Map.keys monkeys)

--reduceWorry :: Monkeys -> Monkeys
--reduceWorry monkeys =
--    where minWorry = maximumBy (\m1 m2 -> compare ()) $
--    TODO: add divBy Monkey, make sure none are dive to less than minWorry which is max divBy of all

monkeyRound :: Maybe Int -> Monkeys -> Int -> Monkeys
monkeyRound supermod monkeys monkeyId = fromMaybe monkeys $ do
    (Monkey is o t insps db) <- Map.lookup monkeyId monkeys
    let updatedItems = map (afterInspect supermod . o) is
    let newMonkeys = foldl (\m item -> throwToMonkey item (t item) m) monkeys updatedItems
    Just $ Map.insert monkeyId (Monkey [] o t (insps + length is) db) newMonkeys

afterInspect :: Maybe Int -> Int -> Int
afterInspect Nothing item = item `div` 3
afterInspect (Just supermod) item = item `mod` supermod

throwToMonkey :: Int -> Int -> Monkeys -> Monkeys
throwToMonkey item = Map.adjust (addItem item)

---TODO: Manipulate inspected items somehow to avoid int overflow
--likely div the respective numbers by something

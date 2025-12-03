module AoC2024.Day3 where

import Debug.Trace (trace)
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC
import qualified Text.Parsec.Combinator as PCOMB
import Text.Read (readMaybe)
import Data.Either (fromRight)
import Data.Maybe (catMaybes)

data Instruction = Mul Int Int
    deriving (Show)

solve :: IO ()
solve = undefined --do
    --fileContents <- readFile "./infiles/AoC2024/Day3.in"
    --let fileContents = "mul(1,2)"
    --let parsed = P.parse allMulsParser "" fileContents
    --print parsed
    --pure ()

upto :: Int -> P.Parsec String () a -> P.Parsec String () [a]
upto n p | n > 0 = (:) <$> P.try p <*> upto (n-1) p P.<|> return []
upto _ _ = return []

upto1 :: Int -> P.Parsec String () a -> P.Parsec String () [a]
upto1 n p | n > 0 = (:) <$> p <*> upto (n-1) p
upto1 _ _ = return []

--allMulsParser :: P.Parsec String () [Maybe Instruction] 
--allMulsParser = P.many $ P.try (mulParser P.<|> (P.anyChar))

mulParser :: P.Parsec String () Instruction
mulParser = do
    PC.string "mul("
    x <- numParser
    P.char ','
    Mul x <$> numParser
        where numParser = do
                        digits <- upto1 3 P.digit
                        maybe (fail "failed to parse num") pure (readMaybe digits)




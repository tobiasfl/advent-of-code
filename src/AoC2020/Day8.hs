
module AoC2020.Day8 where

import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State
import Data.Maybe (fromMaybe, fromJust)
import Data.List (find)

data Instr = Acc Int | Nop Int | Jmp Int
    deriving (Show, Eq)

--TODO: rewrite with lenses
data ExecState = ExecState
    { acc :: Int
    , currLineNr :: Int
    , lineNrHistory :: Set Int
    , lineToFlip :: Maybe Int}
    deriving (Show, Eq)

solve :: IO ()
solve = do
    fileContents <- readFile  "./infiles/AoC2020/Day8.in"
    instructions <- maybe (fail "parse failed") return (parseInstructions fileContents)
    print $ acc (execState (execute instructions) originalState)
    print $ acc (execState (bruteFoce instructions) originalState)

originalState :: ExecState
originalState = ExecState 0 0 S.empty Nothing

parseInstructions :: String -> Maybe [Instr]
parseInstructions = traverse (parseInstr . words) . lines

parseInstr :: [String] -> Maybe Instr
parseInstr [instr, op:xs] = case instr of "acc" -> Acc <$> parsedN
                                          "nop" -> Nop <$> parsedN
                                          "jmp" -> Jmp <$> parsedN
                                          _     -> Nothing
  where parsedN = case op of '-' -> Just $ negate $ read xs
                             '+' -> Just $ read xs
                             _   -> Nothing
        parseN _ = Nothing
parseInstr _ = Nothing

execute :: [Instr] -> State ExecState Bool
execute instrs = do
    (ExecState a cln lnh ltf) <- get
    let isRepetition = S.member cln lnh
    let isEnd = cln == length instrs
    if isRepetition || isEnd then return isEnd else do
        stepMaybeFlip $ instrs !! cln
        execute instrs

step :: Instr -> State ExecState ()
step i = modify (\(ExecState a cln lnh ltf) ->
    case i of (Acc n) -> ExecState (a+n) (cln+1) (S.insert cln lnh) ltf
              (Nop n) -> ExecState a (cln+1) (S.insert cln lnh) ltf
              (Jmp n) -> ExecState a (cln+n) (S.insert cln lnh) ltf)

bruteFoce :: [Instr] -> State ExecState Int
bruteFoce instrs = do
    execute instrs 
    (ExecState a cln lnh ltf) <- get
    let linesToTrySwapping = filter isJmpOrNop $ S.toList lnh
    allResults <- forM linesToTrySwapping (return . runState (execute instrs) . ExecState 0 0 S.empty . Just)
    let (finished, s) = fromJust $ find ((True ==) . fst) allResults
    put s
    return $ acc s
        where isJmpOrNop ln = case instrs !! ln of Nop n -> True
                                                   Jmp n -> True
                                                   _     -> False


stepMaybeFlip :: Instr -> State ExecState ()
stepMaybeFlip i = do
    (ExecState a cln lnh ltf) <- get
    step (maybe i (\lineToFlip -> if cln == lineToFlip then flippedInstr else i) ltf)
        where flippedInstr = case i of Nop n -> Jmp n
                                       Jmp n -> Nop n
                                       _ -> i

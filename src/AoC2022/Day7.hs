module AoC2022.Day7 where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Control.Monad.State
import Control.Monad
import Text.Read (readMaybe)
import Data.Stack

data Files = Map String [Either String Int]
    deriving (Show)

--solve :: IO ()
--solve = do
--    fileContents <- readFile "./infiles/AoC2022/Day7.in"
--    print $ parseDirs (Dir "/" [], ["$ cd test", "$ ls", "$ cd abc", "1234 vhc", "$ cd ..", "$ cd .."])
--    --print $ parseDirs (Dir "/" [], lines fileContents)
--
--parseLine :: String -> State ([String], Files)
--parseLine xs = case words xs of
--    ["$", "ls"]

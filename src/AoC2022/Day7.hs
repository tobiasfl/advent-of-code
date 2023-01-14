module AoC2022.Day7 where

import Data.Multimap (Multimap)
import qualified Data.Multimap as MM
import Data.Maybe (mapMaybe, fromMaybe)
import Control.Monad.State
import Control.Monad
import Text.Read (readMaybe)
import Data.Stack
import Data.Bifunctor (first, second, bimap)
import Data.List (uncons)
import Debug.Trace (trace)

type Path = [String]

data File = File String Int | Dir String
    deriving (Show)

type FileSys = Multimap Path File

solve :: IO ()
solve = do
    fileContents <- readFile "./infiles/AoC2022/Day7.in"
    print $ sumOfReleventDirSizes $ execState parseLines (lines fileContents, [], MM.empty)
    print $ dirToDelete $ execState parseLines (lines fileContents, [], MM.empty)

sumOfReleventDirSizes :: ([String], Path, FileSys) -> Int
sumOfReleventDirSizes (_, _, fs) = sum $ filter (100000 >=) $ MM.foldMapWithKey folder fs
    where folder k v = case v of (Dir dName) -> [findSize v k fs]
                                 _           -> [0]
findSize :: File -> Path -> FileSys -> Int
findSize (Dir dName) path fs = let newPath = dName:path
                                in sum $ map (\f -> findSize f newPath fs) (MM.lookup newPath fs)
findSize (File _ size) _ _ = size

parseLines :: State ([String], Path, FileSys) ()
parseLines = do
    (toParse, path, files) <- get
    let currLine = concat $ take 1 toParse
    let newToParse = drop 1 toParse
    case words currLine of
      ["$", "ls"] -> do
          put (newToParse, path, files)
          parseLines
      ["$", "cd", ".."] -> do
          put (newToParse, drop 1 path, files)
          parseLines
      ["$", "cd", dName] -> do
          put (newToParse, dName:path, MM.insert path (Dir dName) files)
          parseLines
      ["dir", dName] -> do
          put (newToParse, path, files)
          parseLines
      [size, fName] -> do
          put (newToParse, path, MM.insert path (File fName $ read size) files)
          parseLines
      _ -> return ()

--2031851

totalDiskSpace :: Int 
totalDiskSpace = 70000000

requiredSpace :: Int 
requiredSpace = 30000000

dirToDelete :: ([String], Path, FileSys) -> Int
dirToDelete (_, _, fs) = minimum $ filter (requiredSpace - unusedSpace <=) dirSizes
    where folder k v = case v of (Dir dName) -> [findSize v k fs]
                                 _           -> [0]
          sysSize = maximum dirSizes
          dirSizes = MM.foldMapWithKey folder fs
          unusedSpace = totalDiskSpace - sysSize


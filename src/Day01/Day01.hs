module Day01.Day01 (doDay01) where

import Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.IntMap.Lazy as IM
import Data.List (foldl', sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Lib (iCounter, readIntBS8)

{-
Read the lines into list of tuples [(Int, Int)]
Then unzip the list of tuples into a tuple of lists (all left with left, all right with right)

equivalent to:

parseFile filepath = do
  contents <- BS8.readFile filepath
  let (lefts, rights) = unzip (mapMaybe parseLine (BS8.lines contents))
  return (lefts, rights)
-}
parseFile :: FilePath -> IO ([Int], [Int])
parseFile filepath = fmap (unzip . mapMaybe parseLine . BS8.lines) (BS8.readFile filepath)

-- Parses a line into a (Int, Int) tuple if it has exactly two valid ints
parseLine :: BS8.ByteString -> Maybe (Int, Int)
parseLine line = case BS8.words line of -- switch cases the output of BS8.words applied to `line` arg
  [a, b] -> do
    x <- readIntBS8 a
    y <- readIntBS8 b
    return (x, y)
  _ -> Nothing

sumDistances :: Int -> (Int, Int) -> Int
sumDistances acc (a, b) = acc + abs (a - b)

{- Day 1 - Part 1 -}
day01P1 :: FilePath -> IO ()
day01P1 filepath = do
  lists <- parseFile filepath -- [(Int, Int)], need to use <- because it is still in the IO action
  -- sort them both (bimap needed because fmap only applies to the last elem of tuple)
  -- uncurry zip turns zip from List1 -> (List2 -> ZippedList) function to a (List1, List2) -> ZippedList function
  --    have to do this because lists is a tuple of [Int]
  -- then leftwise fold to sum the distances
  print (foldl' sumDistances 0 (uncurry zip (bimap sort sort lists)))

{- Reducer for part 2 leftKey * leftKeyCount * rightKeyCount -}
sumSimilarity :: (IM.IntMap Int, IM.IntMap Int) -> Int
sumSimilarity (leftCounter, rightCounter) = IM.foldlWithKey' go 0 leftCounter
  where
    go :: Int -> IM.Key -> IM.Key -> IM.Key
    go acc key leftCount =
      let rightCount = fromMaybe 0 (IM.lookup key rightCounter)
       in acc + key * leftCount * rightCount

{- Day 1 - Part 2 -}
day01P2 :: FilePath -> IO ()
day01P2 filepath = do
  lists <- parseFile filepath -- [(Int, Int)]
  print (sumSimilarity (bimap iCounter iCounter lists))

{- Function to run Day 1 -}
doDay01 :: IO ()
doDay01 = do
  print "--- Day 01 ---"

  print " -- Part 1"
  day01P1 "src/Day01/day01_small.txt"
  day01P1 "src/Day01/day01.txt"

  print " -- Part 2"
  day01P2 "src/Day01/day01_small.txt"
  day01P2 "src/Day01/day01.txt"

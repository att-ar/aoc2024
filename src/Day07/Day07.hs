module Day07.Day07 (doDay07) where

import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (mapMaybe)
import Lib (readIntBS8)

type Equation = (Int, [Int])

parseEquation :: BS8.ByteString -> Maybe Equation
parseEquation str = case BS8.split ':' str of
  [sRes, sVals] -> do
    -- 1 split only
    res <- readIntBS8 sRes
    vals <- Just $ mapMaybe readIntBS8 (BS8.words sVals)
    return (res, vals)
  _ -> Nothing

parseFile :: FilePath -> IO [Equation]
parseFile filepath = mapMaybe parseEquation . BS8.lines <$> BS8.readFile filepath

backtrack :: Int -> [Int] -> Int -> Bool
backtrack tgt [] cur = tgt == cur
backtrack tgt (val : vals) cur =
  cur <= tgt
    && (backtrack tgt vals (cur * val) || backtrack tgt vals (cur + val))

{- I think I need a backtracking solution, this line is so clean btw -}
day07P1 :: FilePath -> IO ()
day07P1 filepath = print . sum . map fst . filter isPossible =<< parseFile filepath
  where
    isPossible (tgt, val : vals) = backtrack tgt vals val
    isPossible (_, []) = False

backtrackP2 :: Int -> [Int] -> Int -> Bool
backtrackP2 tgt [] cur = tgt == cur
backtrackP2 tgt (val : vals) cur =
  cur <= tgt
    && any
      (backtrackP2 tgt vals) -- curried to accept an integer:
      [cur * val, cur + val, read (show cur ++ show val)]

{- I think I need a backtracking solution, this line is so clean btw -}
day07P2 :: FilePath -> IO ()
day07P2 filepath = print . sum . map fst . filter isPossible =<< parseFile filepath
  where
    isPossible (tgt, val : vals) = backtrackP2 tgt vals val
    isPossible (_, []) = False

doDay07 :: IO ()
doDay07 = do
  print "--- Day 07 ---"

  print " -- Part 1"
  day07P1 "src/Day07/day07_small.txt"
  day07P1 "src/Day07/day07.txt"

  print " -- Part 2"
  day07P2 "src/Day07/day07_small.txt"
  day07P2 "src/Day07/day07.txt"

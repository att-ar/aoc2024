module Day03.Day03 (doDay03) where

import qualified Data.ByteString.Char8 as BS8
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Lib (readIntBS8)
import Text.Regex.TDFA (AllTextMatches, getAllTextMatches, (=~))
import Text.Regex.TDFA.ByteString ()

{-
Parse and compute a "mul(X,Y)" operation
usage of =~ : `my_string =~ my regex`
-}
parseComputeMul :: BS8.ByteString -> Int
parseComputeMul op = product (mapMaybe readIntBS8 (getAllTextMatches (op =~ regex :: AllTextMatches [] BS8.ByteString)))
  where
    -- captures the two 1-3 digit integers to the left and right of the comma
    regex = "[[:digit:]]{1,3}"

{- Parse a line and return a list of mul(X,Y) -}
parseLine :: BS8.ByteString -> [BS8.ByteString]
parseLine line = getAllTextMatches (line =~ regex :: AllTextMatches [] BS8.ByteString)
  where
    regex = "mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)"

{- concatMap concats the results of a map, I need this to prevent getting nested [ [BS8.ByteString] ] -}
day03P1 :: FilePath -> IO ()
day03P1 filepath = print . sum . map parseComputeMul . concatMap parseLine . BS8.lines =<< BS8.readFile filepath

{- Parse a line and return a list of mul(X,Y) -}
parseLineConditionals :: BS8.ByteString -> [BS8.ByteString]
parseLineConditionals line = getAllTextMatches (line =~ regex :: AllTextMatches [] BS8.ByteString)
  where
    mulPat = "mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)"
    dontPat = "don't\\(\\)"
    doPat = "do\\(\\)"
    -- Concat with OR
    regex = mulPat ++ "|" ++ dontPat ++ "|" ++ doPat

{- concatMap concats the results of a map, I need this to prevent getting nested [ [BS8.ByteString] ] -}
day03P2 :: FilePath -> IO ()
day03P2 filepath = do
  contents <- BS8.readFile filepath
  let parsedLines = concatMap parseLineConditionals (BS8.lines contents)

      dont_ = BS8.pack "don't()"
      do_ = BS8.pack "do()"

      reduce :: (Bool, Int) -> BS8.ByteString -> (Bool, Int)
      reduce (state, acc) str
        | str == dont_ = (False, acc) -- instruction parse change state
        | str == do_ = (True, acc) -- instruction parse change state
        | state = (True, acc + parseComputeMul str) -- add mul
        | otherwise = (False, acc) -- skip mul
  print (snd (foldl' reduce (True, 0) parsedLines))

doDay03 :: IO ()
doDay03 = do
  print "--- Day 03 ---"
  print " -- Part 1"
  day03P1 "src/Day03/day03_small_p1.txt"
  day03P1 "src/Day03/day03.txt"

  print " -- Part 2"
  day03P2 "src/Day03/day03_small_p2.txt"
  day03P2 "src/Day03/day03.txt"

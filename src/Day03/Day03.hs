module Day03.Day03 (doDay03) where

import qualified Data.ByteString.Char8 as BS
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Lib (readIntBS)
import Text.Regex.TDFA (AllTextMatches, getAllTextMatches, (=~))
import Text.Regex.TDFA.ByteString ()

{-
Parse and compute a "mul(X,Y)" operation
usage of =~ : `my_string =~ my regex`
-}
parseComputeMul :: BS.ByteString -> Int
parseComputeMul op = product (mapMaybe readIntBS (getAllTextMatches (op =~ regex :: AllTextMatches [] BS.ByteString)))
  where
    -- captures the two 1-3 digit integers to the left and right of the comma
    regex = "[[:digit:]]{1,3}"

{- Parse a line and return a list of mul(X,Y) -}
parseLine :: BS.ByteString -> [BS.ByteString]
parseLine line = getAllTextMatches (line =~ regex :: AllTextMatches [] BS.ByteString)
  where
    regex = "mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)"

{- concatMap concats the results of a map, I need this to prevent getting nested [ [BS.ByteString] ] -}
day03P1 :: FilePath -> IO ()
day03P1 filepath = print . sum . map parseComputeMul . concatMap parseLine . BS.lines =<< BS.readFile filepath

{- Parse a line and return a list of mul(X,Y) -}
parseLineConditionals :: BS.ByteString -> [BS.ByteString]
parseLineConditionals line = getAllTextMatches (line =~ regex :: AllTextMatches [] BS.ByteString)
  where
    mulPat = "mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)"
    dontPat = "don't\\(\\)"
    doPat = "do\\(\\)"
    -- Concat with OR
    regex = mulPat ++ "|" ++ dontPat ++ "|" ++ doPat

{- concatMap concats the results of a map, I need this to prevent getting nested [ [BS.ByteString] ] -}
day03P2 :: FilePath -> IO ()
day03P2 filepath = do
  contents <- BS.readFile filepath
  let parsedLines = concatMap parseLineConditionals (BS.lines contents)

      dont_ = BS.pack "don't()"
      do_ = BS.pack "do()"

      reduce :: (Bool, Int) -> BS.ByteString -> (Bool, Int)
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

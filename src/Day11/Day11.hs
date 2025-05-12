module Day11.Day11 (doDay11) where

import Control.Monad.State.Strict (State, evalState, get, modify')
import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import Lib (readIntBS8)

{- Don't need to use Integer since these results don't exceed 2^63 -}

-- (depth, currentValue) : resultCount
type Memo = HM.HashMap (Int, Int) Int

{- I'm thinking I just do a DFS on each number with 25 recursive layers -}
parseFile :: FilePath -> IO [Int]
parseFile filepath = mapMaybe readIntBS8 . BS8.words <$> BS8.readFile filepath

-- | Number of decimal digits in n (n > 0).
digitCount :: Int -> Int
digitCount n = go n 0
  where
    go 0 0 = 1
    go 0 c = c
    go m c = go (m `quot` 10) (c + 1)

evenDigits :: Int -> Bool
evenDigits = even . digitCount

halve :: Int -> (Int, Int)
halve x =
  let d = digitCount x
      h = d `div` 2
      pow = 10 ^ (d - h)
   in -- ex) x = 1001 -> pow = 2 -> 1001 // 10^2 = 10, 1001 % 100 = 1
      (x `quot` pow, x `mod` pow)

applyRules :: Int -> Int -> Int -> State Memo Int
applyRules maxDepth depth val
  | val == 0 = memoDFS maxDepth (depth + 1) 1 -- stone 0 -> stone 1
  | evenDigits val = do
      let (left, right) = halve val -- stone with even digits is split
      countL <- memoDFS maxDepth (depth + 1) left
      countR <- memoDFS maxDepth (depth + 1) right
      return (countL + countR)
  | otherwise = memoDFS maxDepth (depth + 1) (val * 2024) -- multiply by 2024

{-
when starting at 'depth' and going up to 'maxDepth'.
Needs to be run in the State Monad
-}
memoDFS :: Int -> Int -> Int -> State Memo Int
memoDFS maxDepth depth val
  | depth == maxDepth = return 1 -- base case 1 stone
  | otherwise = do
      memo <- get -- get current state
      let key = (depth, val)

      case HM.lookup key memo of
        Just result -> return result -- seen
        Nothing -> do
          res <- applyRules maxDepth depth val
          -- cache the result (depth, val) : res
          modify' (HM.insert key res)
          return res

{- Share the memo state across the list of starting integers -}
runMemoDFS :: [Int] -> Int -> Int
runMemoDFS nums maxDepth =
  evalState
    ( do
        result <- mapM (memoDFS maxDepth 0) nums
        return $ sum result
    )
    HM.empty

day11 :: Int -> FilePath -> IO ()
day11 maxDepth filepath = do
  nums <- parseFile filepath
  print $ runMemoDFS nums maxDepth

doDay11 :: IO ()
doDay11 = do
  print "--- Day 11 ---"

  print " -- Part 1"
  day11 25 "src/Day11/day11_small.txt"
  day11 25 "src/Day11/day11.txt"

  print " -- Part 2"
  day11 75 "src/Day11/day11_small.txt"
  day11 75 "src/Day11/day11.txt"

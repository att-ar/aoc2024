module Day05.Day05 (doDay05) where

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import qualified Data.Array as A
import Data.Array.ST (STArray, freeze, newArray, readArray, writeArray)
import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IS
import Data.Maybe (mapMaybe)
import Lib (readIntBS)

-- Array of Integer Sets (instead of a map of integer sets)
-- each value set holds the integers that need to appear after the key int
type AfterSet = A.Array Int IS.IntSet

type Edge = (Int, Int)

parseEdge :: BS.ByteString -> Maybe Edge
parseEdge edgeStr = case BS.split '|' edgeStr of
  [a, b] -> do
    before <- readIntBS a
    after <- readIntBS b
    return (before, after)
  _ -> Nothing

parseEdges :: FilePath -> IO [Edge]
parseEdges filepath = mapMaybe parseEdge . BS.lines <$> BS.readFile filepath

parseUpdates :: FilePath -> IO [[Int]]
parseUpdates filepath = map (mapMaybe readIntBS . BS.split ',') . BS.lines <$> BS.readFile filepath

{- N in [10, 99] -}
makeAfterMap :: [Edge] -> AfterSet
makeAfterMap edges = runST $ do
  let maxN = 100
  -- Create a mutable array initialized with empty IntSets
  afterMap <- newArray (0, maxN) IS.empty :: ST s (STArray s Int IS.IntSet)

  -- Iterate through edges and add neighbors to the sets in the mutable array
  -- Update the array entry with an updated set (set insert is not inplace)
  forM_ edges $ \(before, after) ->
    -- readArray afterMap before >>= \currentSet -> writeArray afterMap before (IS.insert after currentSet)
    writeArray afterMap before . IS.insert after =<< readArray afterMap before

  -- Freeze the mutable array into an immutable Array
  freeze afterMap

-- (afterMap, updates) -> Bool
isValidUpdate :: AfterSet -> [Int] -> Bool
isValidUpdate _ [] = True
isValidUpdate afterMap updates = go updates []
  where
    go :: [Int] -> [Int] -> Bool
    go [] _ = True
    go (p : path) visited
      -- the infix is equal to `flip IS.member (edges A.! p)` since it normally takes key first
      | any (`IS.member` (afterMap A.! p)) visited = False
      | otherwise = go path (p : visited)

day05P1 :: FilePath -> FilePath -> IO ()
day05P1 filepathOrder filepathUpdate = do
  afterMap <- makeAfterMap <$> parseEdges filepathOrder
  -- Only odd length in input I think
  let getMiddle lst = lst !! (length lst `div` 2)
  print . sum . map getMiddle . filter (isValidUpdate afterMap) =<< parseUpdates filepathUpdate

doDay05 :: IO ()
doDay05 = do
  print "--- Day 05 ---"

  print " -- Part 1"
  day05P1 "src/Day05/day05_small_order.txt" "src/Day05/day05_small_update.txt"
  day05P1 "src/Day05/day05_order.txt" "src/Day05/day05_update.txt"

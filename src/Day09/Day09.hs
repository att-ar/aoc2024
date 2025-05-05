module Day09.Day09 (doDay09) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntSet as IS
import Data.Maybe (mapMaybe)
import Lib (readIntBS)

type IdxElem = (Int, Maybe Int)

parseLine :: FilePath -> IO [Int]
parseLine filepath = mapMaybe (readIntBS . BS.singleton) . BS.unpack <$> BS.readFile filepath

{- Part 1 : Super clean logic delegation -}

getMemoryLayout :: [Int] -> [Maybe Int]
getMemoryLayout = go 0
  where
    go :: Int -> [Int] -> [Maybe Int]
    go _ [] = []
    go i (x : xs)
      | odd i = replicate x Nothing ++ go (i + 1) xs
      | otherwise = replicate x (Just $ i `div` 2) ++ go (i + 1) xs

findRightmostJust :: [IdxElem] -> Int -> (Maybe Int, [IdxElem])
findRightmostJust [] _ = (Nothing, [])
findRightmostJust ((_, Nothing) : xs) leftIdx = findRightmostJust xs leftIdx
findRightmostJust ((rightIdx, Just x) : xs) leftIdx
  -- equality won't happen: function only called when leftIdx at a Just
  | rightIdx > leftIdx = (Just x, xs)
  | otherwise = (Nothing, [])

compactMemory :: [Maybe Int] -> [Int]
compactMemory memory =
  let -- add index to list
      indexedMemory = zip [0 ..] memory
      rIndexedMemory = reverse indexedMemory

      -- compact the array by moving right units to the empty left ones
      compact :: [IdxElem] -> [IdxElem] -> [Int]
      compact [] _ = [] -- no more space to fill
      compact _ [] = [] -- no more file to move
      compact ((leftIdx, Just m) : ms) (r : rs)
        | rightIdx >= leftIdx = m : compact ms (r : rs)
        | otherwise = []
        where
          rightIdx = fst r
      compact ((leftIdx, Nothing) : ms) rs = case findRightmostJust rs leftIdx of
        (Just fillVal, newRs) -> fillVal : compact ms newRs
        (Nothing, _) -> []
   in compact indexedMemory rIndexedMemory

calcChecksum :: [Int] -> Int
calcChecksum [] = 0
calcChecksum xs = foldr (\(i, x) acc -> acc + i * x) 0 (zip [0 :: Int ..] xs)

day09P1 :: FilePath -> IO ()
day09P1 filepath = do
  print . calcChecksum . compactMemory . getMemoryLayout =<< parseLine filepath

{- Part 2 -}
type MovedSet = IS.IntSet

findRightmostFitFile :: MovedSet -> [IdxElem] -> Int -> IdxElem
findRightmostFitFile _ [] _ = (0, Nothing)
findRightmostFitFile moved ((_, Nothing) : xs) leftIdx = findRightmostFitFile moved xs leftIdx
findRightmostFitFile moved ((rightIdx, Just x) : xs) leftIdx
  | rightIdx <= leftIdx = (0, Nothing)
  | not (odd rightIdx || IS.member rightIdx moved) = (rightIdx, Just x) -- take
  | otherwise = findRightmostFitFile moved xs leftIdx -- skip

{- Very similar to part one but you prepend sub lists and skip indices when recursing, I go sleep tho-}
day09P2 :: FilePath -> IO ()
day09P2 filepath = do
  line <- parseLine filepath
  let indexedLine = zip [0 :: Int ..] line
      rIndexedLine = reverse indexedLine
  print rIndexedLine

-- print $ findRightmostFitFile IS.empty rIndexedLine 3

doDay09 :: IO ()
doDay09 = do
  print "--- Day 09 ---"

  print " -- Part 1"
  day09P1 "src/Day09/day09_smaller.txt"
  day09P1 "src/Day09/day09_small.txt"
  day09P1 "src/Day09/day09.txt"

  print " -- Part 2"
  day09P2 "src/Day09/day09_smaller.txt"

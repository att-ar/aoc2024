module Day08.Day08 (doDay08) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (foldl')
import Lib (pairCombinations)

type Position = (Int, Int)

type PositionSet = HS.HashSet Position

type PositionMap = HM.HashMap Char [Position]

data Accumulator = Accumulator
  { dict :: PositionMap,
    row :: Int,
    col :: Int
  }
  deriving (Show)

parseFile :: FilePath -> IO Accumulator
parseFile filepath = do
  lns <- BS8.lines <$> BS8.readFile filepath

  let -- internal fold over string
      foldString :: Accumulator -> Char -> Accumulator
      foldString acc ch
        | ch /= '.' =
            let updatedPositions = HM.insertWith (++) ch [(row acc, col acc)] $ dict acc
             in acc {dict = updatedPositions, col = col acc + 1} -- take
        | otherwise = acc {col = col acc + 1} -- skip
        -- external fold over list of strings
      foldLines :: Accumulator -> BS8.ByteString -> Accumulator
      foldLines acc str =
        let lineAcc = BS8.foldl' foldString (acc {col = 0}) str
         in -- getting height and width for free
            Accumulator {dict = dict lineAcc, row = row acc + 1, col = col lineAcc}

  return $ foldl' foldLines (Accumulator {dict = HM.empty, row = 0, col = 0}) lns

isInbound :: Int -> Int -> Position -> Bool
isInbound h w (r, c) = r >= 0 && r < h && c >= 0 && c < w

{- Part 1 - Naive pair creation with recursion -}

calculateAntinodesForPair :: ((Int, Int) -> Bool) -> Position -> Position -> [Position]
calculateAntinodesForPair inbound (r1, c1) (r2, c2) =
  let dr = r2 - r1
      dc = c2 - c1
   in -- A-V or B+V where V=B-A
      filter inbound [(r1 - dr, c1 - dc), (r2 + dr, c2 + dc)]

-- Process all pairs in a [Position]
processPositionList :: (Int, Int) -> [Position] -> PositionSet -> PositionSet
processPositionList _ [] accumulatedAntinodes = accumulatedAntinodes -- Base case 1: No elements left
processPositionList _ [_] accumulatedAntinodes = accumulatedAntinodes -- Base case 2: Only one element left, no pairs possible
processPositionList dims@(height, width) (p1 : ps) accumulatedAntinodes =
  let -- Calculate all antinodes formed by pairing p1 with each element in ps
      calcAntinodesWithPos :: Position -> [Position]
      calcAntinodesWithPos = calculateAntinodesForPair (isInbound height width) p1

      newAntinodesFromPos :: PositionSet
      newAntinodesFromPos =
        foldl'
          (\accSet p2 -> foldl' (flip HS.insert) accSet $ calcAntinodesWithPos p2)
          HS.empty -- empty set for current p1
          ps

      combinedAntinodes = HS.union accumulatedAntinodes newAntinodesFromPos
   in processPositionList dims ps combinedAntinodes -- recurse to next p1 in ps

day08P1 :: FilePath -> IO ()
day08P1 filepath = do
  contents <- parseFile filepath

  let height = row contents
      width = col contents
      charPos = dict contents

      getAntinodesOfLetter :: [Position] -> PositionSet
      getAntinodesOfLetter positions = processPositionList (height, width) positions HS.empty

      getAllAntinodes :: [Position] -> PositionSet -> PositionSet
      getAllAntinodes positions accSet = HS.union accSet $ getAntinodesOfLetter positions

      antinodes = HM.foldr' getAllAntinodes HS.empty charPos

  print $ HS.size antinodes

{- Part 2 - modified to use pairCombinations from Lib and fewer set operations -}

subtractPositions :: Position -> Position -> (Int, Int)
subtractPositions (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

addPositions :: Position -> Position -> (Int, Int)
addPositions (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- Generate antinodes outwards from p along vector v as long as they are inbound
walkVector :: (Position -> Bool) -> Position -> (Int, Int) -> [Position]
walkVector inbound p v
  | not $ inbound nextAntinode = []
  | otherwise = nextAntinode : walkVector inbound nextAntinode v
  where
    nextAntinode = addPositions p v

getPairwiseAntinodes :: (Position -> Bool) -> Position -> Position -> [Position]
getPairwiseAntinodes inbound p1 p2 =
  let (dx, dy) = subtractPositions p1 p2
   in walkVector inbound p1 (-dx, -dy) ++ walkVector inbound p2 (dx, dy)

-- Processes a list of positions belonging to a single character.
-- Generates distinct antinodes from all pairs and includes the original positions
-- if the list has at least two elements.
getGroupwiseAntinodes :: (Int, Int) -> [Position] -> PositionSet
getGroupwiseAntinodes (height, width) positions = HS.fromList (antennas ++ allPairwiseAntinodes)
  where
    inbound = isInbound height width

    combinePairwiseAntinodes :: (Position, Position) -> [Position] -> [Position]
    combinePairwiseAntinodes (p1, p2) acc = acc ++ getPairwiseAntinodes inbound p1 p2

    allPairwiseAntinodes :: [Position]
    allPairwiseAntinodes = foldr combinePairwiseAntinodes [] $ pairCombinations positions

    antennas :: [Position]
    antennas = case positions of
      (_ : _ : _) -> positions -- List has 2+ elems
      _ -> []

day08P2 :: FilePath -> IO ()
day08P2 filepath = do
  contents <- parseFile filepath
  let dims = (row contents, col contents)
      charPos = dict contents
      -- (f . g) x = f (g x)
      antinodes = HM.foldr (HS.union . getGroupwiseAntinodes dims) HS.empty charPos
  print $ HS.size antinodes

doDay08 :: IO ()
doDay08 = do
  print "--- Day 08 ---"

  print " -- Part 1"
  day08P1 "src/Day08/day08_small.txt"
  day08P1 "src/Day08/day08.txt"

  print " -- Part 2"
  day08P2 "src/Day08/day08_small.txt"
  day08P2 "src/Day08/day08.txt"

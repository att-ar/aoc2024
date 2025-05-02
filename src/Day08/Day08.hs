module Day08.Day08 (doDay08) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (foldl')

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
  lns <- BS.lines <$> BS.readFile filepath

  let foldString :: Accumulator -> Char -> Accumulator
      foldString acc ch
        | ch /= '.' =
            let updatedPositions = HM.insertWith (++) ch [(row acc, col acc)] $ dict acc
             in acc {dict = updatedPositions, col = col acc + 1} -- take
        | otherwise = acc {col = col acc + 1} -- skip
      foldLines :: Accumulator -> BS.ByteString -> Accumulator
      foldLines acc str =
        let lineAcc = BS.foldl' foldString (acc {col = 0}) str
         in -- getting height and width for free
            Accumulator {dict = dict lineAcc, row = row acc + 1, col = col lineAcc}

  return $ foldl' foldLines (Accumulator {dict = HM.empty, row = 0, col = 0}) lns

isInbound :: Int -> Int -> Position -> Bool
isInbound h w (r, c) = r >= 0 && r < h && c >= 0 && c < w

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
      calcAntinodesWithP1 :: Position -> [Position]
      calcAntinodesWithP1 = calculateAntinodesForPair (isInbound height width) p1

      newAntinodesFromPos :: PositionSet
      newAntinodesFromPos =
        foldl'
          (\accSet p2 -> foldl' (flip HS.insert) accSet $ calcAntinodesWithP1 p2)
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

doDay08 :: IO ()
doDay08 = do
  print "--- Day 08 ---"

  print " -- Part 1"
  day08P1 "src/Day08/day08_small.txt"
  day08P1 "src/Day08/day08.txt"

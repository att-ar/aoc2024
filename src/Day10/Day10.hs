module Day10.Day10 (doDay10) where

import qualified Data.ByteString.Char8 as BS8
import Data.Char (digitToInt)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Lib (readIntBS8)

type Position = (Int, Int)

type PositionDelta = (Int, Int)

type PositionSet = HS.HashSet Position

type Score = Int

data Matrix = Matrix
  { height :: Int,
    width :: Int,
    matrix :: V.Vector (VU.Vector Int)
  }
  deriving (Show)

directions :: [PositionDelta]
directions = [(1, 0), (0, 1), (-1, 0), (0, -1)]

(!) :: Matrix -> Position -> Maybe Int
(!) grid (row, col) =
  if row >= 0 && row < height grid && col >= 0 && col < width grid
    then Just $ matrix grid V.! row VU.! col
    else Nothing

parseLine :: BS8.ByteString -> VU.Vector Int
parseLine bstr = VU.fromList $ BS8.foldr' (\ch acc -> digitToInt ch : acc) [] bstr

{- Make a matrix of integers -}
parseFile :: FilePath -> IO ([Position], Matrix)
parseFile filepath = do
  vecs <- (V.fromList . map parseLine) . BS8.lines <$> BS8.readFile filepath

  let h = V.length vecs
      w = VU.length $ V.head vecs
      m = Matrix {height = h, width = w, matrix = vecs}

      isZero i j = case m ! (i, j) of
        Just num -> num == 0
        Nothing -> False

      zeros :: [Position]
      zeros = [(i, j) | i <- [0 .. h - 1], j <- [0 .. w - 1], isZero i j]

  return (zeros, m)

{-
I can return the score from each position so that future DFS from new trailheads can early exit a path
That's a little extra for now, good practice though because I have to maintain an input-wise hashmap alongside a dfs-wise hashset
TODO: use a hashmap storing position's scores to early exit paths
-}
getTrailheadScore :: Matrix -> Int -> PositionSet -> Position -> (Score, PositionSet)
getTrailheadScore mat cur visited pos@(r, c)
  | HS.member pos visited = (0, visited)
  | mat ! (r, c) == Just 9 = (1, newVisited)
  | otherwise =
      let next = cur + 1
          validNextPositions = filter (\p -> mat ! p == Just next) [(r + dr, c + dc) | (dr, dc) <- directions]

          go :: Position -> (Score, PositionSet) -> (Score, PositionSet)
          go p (score, vis) =
            let (newScore, newVis) = getTrailheadScore mat next vis p
             in (newScore + score, newVis)
       in foldr go (0, newVisited) validNextPositions
  where
    newVisited = HS.insert pos visited

day10P1 :: FilePath -> IO ()
day10P1 filepath = do
  (zeros, mat) <- parseFile filepath
  print $ foldr (\pos acc -> fst (getTrailheadScore mat 0 HS.empty pos) + acc) 0 zeros

{-
Part 2 is part 1 without a visited set lol
but more efficient is to just use the hashmap i mentioned earlier
-}
type Rating = Int

type PositionRatingMap = HM.HashMap Position Rating

getTrailheadRating :: Matrix -> PositionRatingMap -> Int -> Position -> (Rating, PositionRatingMap)
getTrailheadRating mat posmap cur pos@(r, c)
  | Just rating <- HM.lookup pos posmap = (rating, posmap) -- since this is checked first, I need to use `validNextPositions`
  | mat ! pos == Just 9 = (1, posmap)
  | otherwise =
      let next = cur + 1
          validNextPositions = filter (\p -> mat ! p == Just next) [(r + dr, c + dc) | (dr, dc) <- directions]

          go :: Position -> (Rating, PositionRatingMap) -> (Rating, PositionRatingMap)
          go p (rating, pmap) =
            let (newRating, newPmap) = getTrailheadRating mat pmap next p
             in (newRating + rating, newPmap)

          (positionRating, positionMap) = foldr go (0, posmap) validNextPositions
       in (positionRating, HM.insert pos positionRating positionMap)

day10P2 :: FilePath -> IO ()
day10P2 filepath = do
  (zeros, mat) <- parseFile filepath
  -- mapM_ (print . fst . getTrailheadRating mat HM.empty 0) zeros
  let go :: Position -> (Rating, PositionRatingMap) -> (Rating, PositionRatingMap)
      go pos (rating, posmap) = (rating + trailheadRating, updatePosmap)
        where
          (trailheadRating, updatePosmap) = getTrailheadRating mat posmap 0 pos
  print $ fst $ foldr go (0, HM.empty) zeros

doDay10 :: IO ()
doDay10 = do
  print "--- Day 10 ---"

  print " -- Part 1"
  day10P1 "src/Day10/day10_small.txt"
  day10P1 "src/Day10/day10.txt"

  day10P2 "src/Day10/day10_small.txt"
  day10P2 "src/Day10/day10.txt"

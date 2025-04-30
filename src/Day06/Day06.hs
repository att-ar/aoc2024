module Day06.Day06 (doDay06) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashSet as HS
import Data.Maybe (fromJust)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Lib (byteStringToUnboxedVector) -- , timeIt)

type Position = (Int, Int)

type PositionDelta = (Int, Int)

type PositionSet = HS.HashSet Position

type PositionVector = (Position, PositionDelta)

type PositionVectorSet = HS.HashSet PositionVector

data CharMatrix = CharMatrix
  { height :: Int,
    width :: Int,
    matrix :: V.Vector (VU.Vector Char)
  }
  deriving (Show)

{- Return ' ' on out of bounds because it works for this problem -}
(!) :: CharMatrix -> Position -> Maybe Char
(!) grid (row, col) =
  if row >= 0 && row < height grid && col >= 0 && col < width grid
    then Just $ matrix grid V.! row VU.! col
    else Nothing

-- the outer vector needs to be boxed because type metadata is needed since the elements aren't a primitive
parseGrid :: FilePath -> IO CharMatrix
parseGrid filepath = do
  m <- V.fromList . map byteStringToUnboxedVector . BS.lines <$> BS.readFile filepath
  let h = V.length m
      w = VU.length $ V.head m
  return CharMatrix {height = h, width = w, matrix = m}

findStart :: V.Vector (VU.Vector Char) -> Maybe Position
findStart grid =
  V.foldr'
    ( \(rowIdx, row) acc ->
        case acc of
          Just _ -> acc -- 'X' already found
          Nothing ->
            case VU.findIndex (== '^') row of
              Just colIdx -> Just (rowIdx, colIdx)
              _ -> Nothing
    )
    Nothing -- starting acc
    (V.indexed grid)

getNewDirection :: PositionDelta -> PositionDelta
getNewDirection delta = case delta of
  (-1, 0) -> (0, 1)
  (0, 1) -> (1, 0)
  (1, 0) -> (0, -1)
  (0, -1) -> (-1, 0)
  _ -> (0, 0) -- placeholder

dayO6P1 :: FilePath -> IO ()
dayO6P1 filepath = do
  grid <- parseGrid filepath

  -- (visited, curPos, dir) -> PositionSet
  let simulateGuard :: PositionSet -> Position -> PositionDelta -> Int
      simulateGuard visited (row, col) (dr, dc) = case grid ! (row, col) of
        Nothing -> HS.size visited
        Just '#' -> simulateGuard visited (row - dr, col - dc) (getNewDirection (dr, dc))
        _ -> simulateGuard (HS.insert (row, col) visited) (row + dr, col + dc) (dr, dc)

      startPos = fromJust $ findStart $ matrix grid
      startDir = (-1, 0) :: PositionDelta

  print $ simulateGuard HS.empty startPos startDir

{-
Key insight: revisiting a position while travelling in the same dir means you are in loop
Need to reduce the search space
-}
day06P2 :: FilePath -> IO ()
day06P2 filepath = do
  grid <- parseGrid filepath
  let simulateGuard :: PositionSet -> Position -> PositionDelta -> PositionSet
      simulateGuard visited (row, col) (dr, dc) = case grid ! (row, col) of
        Nothing -> visited
        Just '#' -> simulateGuard visited (row - dr, col - dc) (getNewDirection (dr, dc))
        _ -> simulateGuard (HS.insert (row, col) visited) (row + dr, col + dc) (dr, dc)

      startPos = fromJust $ findStart $ matrix grid
      startDir = (-1, 0) :: PositionDelta

      guardPositions = simulateGuard HS.empty startPos startDir

      trimmedObstaclePositions =
        filter
          (\obstacle -> fromJust (grid ! obstacle) /= '#' && HS.member obstacle guardPositions && obstacle /= startPos)
          [(obsR, obsC) | obsR <- [0 .. height grid - 1], obsC <- [0 .. width grid - 1]]

      isLoop :: PositionVectorSet -> Position -> Position -> PositionDelta -> Bool
      isLoop visitedVec obstacle (row, col) (dr, dc)
        | HS.member ((row, col), (dr, dc)) visitedVec = True -- loop
        | otherwise = case grid ! (row, col) of
            Nothing -> False -- exited grid
            Just ch ->
              if ch == '#' || (row, col) == obstacle
                then isLoop visitedVec obstacle (row - dr, col - dc) (getNewDirection (dr, dc))
                else isLoop (HS.insert ((row, col), (dr, dc)) visitedVec) obstacle (row + dr, col + dc) (dr, dc)

  print $ length $ filter (\obstacle -> isLoop HS.empty obstacle startPos startDir) trimmedObstaclePositions

{-
I will try to run the obstacle simulations without restarting from the start pos everytime
-}
day06P2Faster :: FilePath -> IO ()
day06P2Faster filepath = do
  grid <- parseGrid filepath

  let startPos = fromJust $ findStart $ matrix grid
      startDir = (-1, 0) :: PositionDelta

      -- (prefixVisited, obstacle, startPos, startDir) -> isLoopBool
      simulateObstacle :: PositionVectorSet -> Position -> Position -> PositionDelta -> Bool
      simulateObstacle visitedVec obstacle (row, col) (dr, dc)
        | HS.member ((row, col), (dr, dc)) visitedVec = True -- loop
        | otherwise = case grid ! (row, col) of
            Nothing -> False -- exited grid
            Just ch ->
              if ch == '#' || (row, col) == obstacle
                then simulateObstacle visitedVec obstacle (row - dr, col - dc) (getNewDirection (dr, dc))
                else simulateObstacle (HS.insert ((row, col), (dr, dc)) visitedVec) obstacle (row + dr, col + dc) (dr, dc)

      isPositionValid :: Position -> Bool
      isPositionValid pos = maybe False (/= '#') (grid ! pos)

      isObstacleNew :: PositionSet -> Position -> Bool
      isObstacleNew visited obstacle = not $ HS.member obstacle visited

      {-
      Will reduce redundant work by placing an obstacle in front of the guard at every iteration that I can
        i.e. in bound and not already obstacle
      We know that walking the original path will never loop, so terminate when out of bounds
      -}
      simulateAllObstacles :: PositionVectorSet -> PositionSet -> Position -> PositionDelta -> Int
      simulateAllObstacles visitedVec visited (row, col) (dr, dc) = case grid ! (row, col) of
        Nothing -> 0 -- finish sim
        Just '#' -> simulateAllObstacles visitedVec visited (row - dr, col - dc) (getNewDirection (dr, dc)) -- go along proper path
        _ -> do
          -- simulate obstacle if appropriate
          let newVisitedVec = HS.insert ((row, col), (dr, dc)) visitedVec
              newVisited = HS.insert (row, col) visited
              newObstacle = (row + dr, col + dc)
              (ndr, ndc) = getNewDirection (dr, dc)

              simResult =
                fromEnum $
                  isPositionValid newObstacle
                    && isObstacleNew newVisited newObstacle
                    && simulateObstacle newVisitedVec newObstacle (row, col) (ndr, ndc)

          simResult + simulateAllObstacles newVisitedVec newVisited (row + dr, col + dc) (dr, dc)

  print $ simulateAllObstacles HS.empty HS.empty startPos startDir

doDay06 :: IO ()
doDay06 = do
  print "--- Day 06 ---"

  print " -- Part 1"
  dayO6P1 "src/Day06/day06_small.txt"
  dayO6P1 "src/Day06/day06.txt"

  print " -- Part 2"
  day06P2 "src/Day06/day06_small.txt"
  -- timeIt $ day06P2 "src/Day06/day06.txt"

  print " -- Part 2 : Faster"
  day06P2Faster "src/Day06/day06_small.txt"

-- timeIt $ day06P2Faster "src/Day06/day06.txt"

module Day04.Day04 (doDay04) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- (rows, cols, unboxed vector of char)
type CharMatrix = (Int, Int, VU.Vector Char)

type Position = (Int, Int)

type PositionDelta = (Int, Int)

type XPositions = VU.Vector Position

makeMatrix :: Int -> Int -> VU.Vector Char -> CharMatrix
makeMatrix rows cols elems = (rows, cols, elems)

{-
  O(1) access function
  Out of Bounds checks, returns ' ' on failure makes my problem logic easier
-}
(!) :: CharMatrix -> (Int, Int) -> Char
(!) (rows, cols, vec) (r, c)
  | r < 0 || r >= rows || c < 0 || c >= cols = ' '
  | otherwise = vec VU.! (r * cols + c)

{-
parse File into my CharMatrix type
Avoids making an intermediate [Char] and just forms the Vector straight away
-}
parseFileMatrix :: FilePath -> IO (CharMatrix, XPositions)
parseFileMatrix path = do
  -- using a boxed vector for the lines
  lns <- V.fromList . BS.lines <$> BS.readFile path
  let rows = V.length lns :: Int
      cols = BS.length (lns V.! 1) :: Int

      -- unboxed vectors for the int vectors
      -- build flat vector repr of the matrix input
      flatVec =
        VU.generate -- :: Int -> (Int -> ByteString) -> Vector ByteString
          (rows * cols) -- equivalent to the `end` of `range` so it's upper bound
          ( \i ->
              let (r, c) = i `divMod` cols -- divMod returns the quotient and remainder
               in BS.index (lns V.! r) c -- index into rth row and cth col
          )
      -- build a vector with the positions of all 'X's
      -- can also use VU.unfoldr which skips an intermediate vector
      xPos =
        VU.fromList
          [ (r, c)
            | i <- [0 .. rows * cols - 1],
              let ch = flatVec VU.! i,
              ch == 'X',
              let (r, c) = i `divMod` cols
          ]
  return (makeMatrix rows cols flatVec, xPos)

{- (matrix, (row,col), (rDir, cDir), targetLetter) -> Bool -}
dfsMatrix :: CharMatrix -> Position -> PositionDelta -> Char -> Bool
dfsMatrix grid (row, col) (rDir, cDir) tgtLetter
  | tgtLetter == 'S' = grid ! (row, col) == tgtLetter
  | tgtLetter == 'A' = grid ! (row, col) == 'A' && dfsMatrix grid (row + rDir, col + cDir) (rDir, cDir) 'S'
  | tgtLetter == 'M' = grid ! (row, col) == 'M' && dfsMatrix grid (row + rDir, col + cDir) (rDir, cDir) 'A'
  | otherwise = False -- invalid input

day04P1 :: FilePath -> IO ()
day04P1 filepath = do
  (matrix, xPos) <- parseFileMatrix filepath
  let dfs = dfsMatrix matrix -- partial evaluation
      dfsOnX :: Position -> Int
      dfsOnX (r, c) =
        fromEnum (dfs (r + 1, c) (1, 0) 'M') -- down
          + fromEnum (dfs (r - 1, c) (-1, 0) 'M') -- up
          + fromEnum (dfs (r, c + 1) (0, 1) 'M') -- right
          + fromEnum (dfs (r, c - 1) (0, -1) 'M') -- left
          + fromEnum (dfs (r + 1, c + 1) (1, 1) 'M') -- diag down right
          + fromEnum (dfs (r + 1, c - 1) (1, -1) 'M') -- diag down left
          + fromEnum (dfs (r - 1, c + 1) (-1, 1) 'M') -- diag up right
          + fromEnum (dfs (r - 1, c - 1) (-1, -1) 'M') -- diag up left
  print . VU.sum . VU.map dfsOnX $ xPos

doDay04 :: IO ()
doDay04 = do
  print "--- Day 04 ---"

  print " -- Part 1"
  day04P1 "src/Day04/day04_small.txt"

  print " -- Part 2"
  day04P1 "src/Day04/day04.txt"

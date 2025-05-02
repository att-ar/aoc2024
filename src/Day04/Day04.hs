module Day04.Day04 (doDay04) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

-- (rows, cols, unboxed vector of char)
type CharMatrix = (Int, Int, VU.Vector Char)

type Position = (Int, Int)

type PositionDelta = (Int, Int)

type XPositions = V.Vector Position

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
Return the matrix and the vector of positions for the given `letter`
-}
parseFileMatrix :: FilePath -> Char -> IO (CharMatrix, XPositions)
parseFileMatrix path letter = do
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
      -- build a vector with the positions of all '`letter`'s
      -- can also use VU.unfoldr which skips an intermediate vector
      letterPos =
        V.fromList
          [ (r, c)
            | i <- [0 .. rows * cols - 1],
              let ch = flatVec VU.! i,
              ch == letter,
              let (r, c) = i `divMod` cols
          ]
  return (makeMatrix rows cols flatVec, letterPos)

{- (matrix, (row,col), (drow, dcol), targetLetter) -> Bool -}
dfsMatrix :: CharMatrix -> Position -> PositionDelta -> Char -> Bool
dfsMatrix grid (row, col) (drow, dcol) tgtLetter
  | tgtLetter == 'S' = grid ! (row, col) == tgtLetter
  | tgtLetter == 'A' = grid ! (row, col) == 'A' && dfsMatrix grid (row + drow, col + dcol) (drow, dcol) 'S'
  | tgtLetter == 'M' = grid ! (row, col) == 'M' && dfsMatrix grid (row + drow, col + dcol) (drow, dcol) 'A'
  | otherwise = False -- invalid input

day04P1 :: FilePath -> IO ()
day04P1 filepath = do
  (matrix, xPos) <- parseFileMatrix filepath 'X'
  let dfsOnX (r, c) = sum [fromEnum $ dfsMatrix matrix (r + dr, c + dc) (dr, dc) 'M' | dr <- [-1, 0, 1], dc <- [-1, 0, 1]]
  print . V.sum . V.map dfsOnX $ xPos

checkPosMAS :: CharMatrix -> Position -> Bool
checkPosMAS grid (row, col) =
  or [grid ! (row - d, col - d) == 'M' && grid ! (row + d, col + d) == 'S' | d <- [-1, 1]] -- downslop diag
    && or [grid ! (row + d, col - d) == 'M' && grid ! (row - d, col + d) == 'S' | d <- [-1, 1]] -- upslope diag

day04P2 :: FilePath -> IO ()
day04P2 filepath = do
  -- This time I want the positions of all 'A's
  (matrix, aPos) <- parseFileMatrix filepath 'A'
  let checkPosMASWithMatrix = checkPosMAS matrix
  print . V.sum . V.map (fromEnum . checkPosMASWithMatrix) $ aPos

doDay04 :: IO ()
doDay04 = do
  print "--- Day 04 ---"

  print " -- Part 1"
  day04P1 "src/Day04/day04_small.txt"
  day04P1 "src/Day04/day04.txt"

  print " -- Part 2"
  day04P2 "src/Day04/day04_small.txt"
  day04P2 "src/Day04/day04.txt"

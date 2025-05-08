module Day09.Day09 (doDay09) where

import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (mapMaybe)
import Lib (readIntBS8)

type IdxElem = (Int, Maybe Int)

parseLine :: FilePath -> IO [Int]
parseLine filepath = mapMaybe (readIntBS8 . BS8.singleton) . BS8.unpack <$> BS8.readFile filepath

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
      -- file:
      compact ((leftIdx, Just m) : ms) (r : rs)
        | rightIdx >= leftIdx = m : compact ms (r : rs)
        | otherwise = []
        where
          rightIdx = fst r
      -- space:
      compact ((leftIdx, Nothing) : ms) rs = case findRightmostJust rs leftIdx of
        (Just fillVal, newRs) -> fillVal : compact ms newRs
        (Nothing, _) -> []
   in compact indexedMemory rIndexedMemory

calcChecksum :: [Int] -> Int
calcChecksum xs = foldr (\(i, x) acc -> acc + i * x) 0 (zip [0 :: Int ..] xs)

day09P1 :: FilePath -> IO ()
day09P1 filepath = do
  print . calcChecksum . compactMemory . getMemoryLayout =<< parseLine filepath

{-
Part 2

I will only maintain the active state of the remaining memory I need to update on every recursion

Whenever I move a file I will delete it from the active state (pure)

That's really all I need to do. Different solution from part 1 since I'm not using two pointers.

CHANGE: The problem wants you to prioritize moving the rightmost file, not fill in the leftmost space

So I need to flip my logic

-}
type FileId = Int

type Size = Int

type SpaceWrong = Size

type File = (FileId, Size)

type BlockWrong = Either SpaceWrong File

calcChecksumMaybe :: [Maybe Int] -> Int
calcChecksumMaybe xs = foldr checksumMaybe 0 (zip [0 :: Int ..] xs)
  where
    checksumMaybe (_, Nothing) acc = acc
    checksumMaybe (i, Just x) acc = acc + i * x

{-
Incorrect Algo: Trying to fill in spaces greedily
-}

toBlockWrong :: Int -> Int -> BlockWrong
toBlockWrong i val
  | odd i = Left val
  | otherwise = Right (i `div` 2, val)

freeMemory :: FileId -> [BlockWrong] -> [BlockWrong]
freeMemory targetId = map go
  where
    go :: BlockWrong -> BlockWrong
    go block@(Left _) = block
    go block@(Right (fileId, size))
      | fileId == targetId = Left size
      | otherwise = block

combineSpaces :: [BlockWrong] -> [BlockWrong]
combineSpaces = foldr step []
  where
    step (Right file) acc = Right file : acc
    step (Left space) (Left space' : rest) = Left (space + space') : rest
    step (Left space) acc = Left space : acc

findRightmostFitFile :: [BlockWrong] -> SpaceWrong -> Maybe File
findRightmostFitFile blocks space = case reverse [file | Right file@(_, size) <- blocks, size <= space] of
  (f : _) -> Just f
  [] -> Nothing

compactMemoryNoFragmentWrong :: [Int] -> [Maybe Int]
compactMemoryNoFragmentWrong nums = do
  let memoryBlocks :: [BlockWrong]
      memoryBlocks = zipWith toBlockWrong [0 :: Int ..] nums

      compact :: [BlockWrong] -> [Maybe Int]
      compact [] = []
      -- file:
      compact ((Right (fId, fSize)) : blocks) = replicate fSize (Just fId) ++ compact blocks
      -- space:
      compact ((Left space) : blocks) = case findRightmostFitFile blocks space of
        Nothing -> replicate space Nothing ++ compact blocks -- not enough space
        Just (fId, fSize) ->
          let toPrepend = replicate fSize (Just fId)
              remainingSpace = space - fSize
              cleanedBlocks = combineSpaces $ freeMemory fId blocks
              updatedBlocks = if remainingSpace > 0 then Left remainingSpace : cleanedBlocks else cleanedBlocks
           in toPrepend ++ compact updatedBlocks
   in compact memoryBlocks

day09P2Wrong :: FilePath -> IO ()
day09P2Wrong filepath = print . calcChecksumMaybe . compactMemoryNoFragmentWrong =<< parseLine filepath

{- Correct Algo: Trying to Move files greedily (does the reverse) -}

type Index = Int

type Space = (Index, Size)

type Block = Either Space File

toBlock :: Int -> Int -> Block
toBlock i val
  | odd i = Left (i, val)
  | otherwise = Right (i `div` 2, val)

-- input is reversed memory
findLeftmostFitSpace :: [Block] -> Size -> Maybe Space
findLeftmostFitSpace rBlocks size = case reverse [space | (Left space) <- rBlocks, size <= snd space] of
  (s : _) -> Just s
  [] -> Nothing

{-
Assumes that the space to write to was found using findLeftmostFitSpace.
Auto fills in the remaining space (to the right of the file in forward traversal).
Remember that we are solving this in a reverse traversal, so we put the space on the left.
-}
writeMemory :: File -> Int -> [Block] -> [Block]
writeMemory file@(_, fSize) sIdx = go
  where
    go :: [Block] -> [Block]
    go [] = []
    go (block@(Right _) : blocks) = block : go blocks
    go (block@(Left (idx, space)) : blocks)
      | sIdx /= idx = block : go blocks
      | space == fSize = Right file : go blocks
      | otherwise = Left (idx, space - fSize) : Right file : go blocks

{- Traverse backwards since we want to move files not fill in spaces -}
compactMemoryNoFragment :: [Int] -> [Maybe Int]
compactMemoryNoFragment nums = do
  let rMemoryBlocks :: [Block]
      rMemoryBlocks = reverse $ zipWith toBlock [0 :: Int ..] nums

      compact :: [Block] -> [Maybe Int]
      compact [] = []
      -- space:
      compact (Left (_, space) : rBlocks) = replicate space Nothing ++ compact rBlocks
      -- file:
      compact ((Right file@(fId, fSize)) : rBlocks) = case findLeftmostFitSpace rBlocks fSize of
        Nothing -> replicate fSize (Just fId) ++ compact rBlocks -- nowhere to move file
        Just (idx, _) ->
          let toPrepend = replicate fSize Nothing
              updatedRBlocks = writeMemory file idx rBlocks
           in toPrepend ++ compact updatedRBlocks
   in reverse $ compact rMemoryBlocks -- reverse back to normal order

day09P2 :: FilePath -> IO ()
day09P2 filepath = print . calcChecksumMaybe . compactMemoryNoFragment =<< parseLine filepath

doDay09 :: IO ()
doDay09 = do
  print "--- Day 09 ---"

  print " -- Part 1"
  day09P1 "src/Day09/day09_smaller.txt"
  day09P1 "src/Day09/day09_small.txt"
  day09P1 "src/Day09/day09.txt"

  print " -- Part 2"
  print " -- Priority: fill spaces"
  day09P2Wrong "src/Day09/day09_smaller.txt"
  day09P2Wrong "src/Day09/day09_small.txt"
  day09P2Wrong "src/Day09/day09.txt"

  print " -- Priority: move files"
  day09P2 "src/Day09/day09_smaller.txt"
  day09P2 "src/Day09/day09_small.txt"
  day09P2 "src/Day09/day09.txt"

module Lib (readIntBS, aCounter, iCounter) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Map.Strict as Map

-- Helper to parse a ByteString into Int safely
readIntBS :: BS.ByteString -> Maybe Int
readIntBS bs = fmap fst (BS.readInt bs)

aCounter :: (Ord a) => [a] -> Map.Map a Int
aCounter xs = Map.fromListWith (+) [(x, 1) | x <- xs]

iCounter :: [Int] -> IntMap.IntMap Int
iCounter xs = IntMap.fromListWith (+) [(x, 1) | x <- xs]

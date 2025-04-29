module Lib (readIntBS, aCounter, iCounter, textToUnboxedVector, byteStringToUnboxedVector) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

-- Helper to parse a ByteString into Int safely
readIntBS :: BS.ByteString -> Maybe Int
readIntBS bs = fmap fst (BS.readInt bs)

aCounter :: (Ord a) => [a] -> Map.Map a Int
aCounter xs = Map.fromListWith (+) [(x, 1) | x <- xs]

iCounter :: [Int] -> IntMap.IntMap Int
iCounter xs = IntMap.fromListWith (+) [(x, 1) | x <- xs]

textToUnboxedVector :: T.Text -> VU.Vector Char
textToUnboxedVector = VU.fromList . T.unpack

byteStringToUnboxedVector :: BS.ByteString -> VU.Vector Char
byteStringToUnboxedVector = VU.fromList . BS.unpack

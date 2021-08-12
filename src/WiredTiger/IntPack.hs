{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WiredTiger.IntPack (
  getInt,
  getWord,
  putInt,
  putWord
) where

import Control.Monad
import Data.Bits
import Data.Serialize
import Data.Word


negMultiMarker, neg2ByteMarker, neg1ByteMarker, pos1ByteMarker, pos2ByteMarker, posMultiMarker :: Word8

negMultiMarker = 0x10
neg2ByteMarker = 0x20
neg1ByteMarker = 0x40
pos1ByteMarker = 0x80
pos2ByteMarker = 0xc0
posMultiMarker = 0xe0

neg2ByteMin, neg1ByteMin :: Int
pos1ByteMax, pos2ByteMax :: Word

neg1ByteMin = -1 `shiftL` 6
neg2ByteMin = -1 `shiftL` 13 + neg1ByteMin
pos1ByteMax = 1 `shiftL` 6 - 1
pos2ByteMax = 1 `shiftL` 13 + pos1ByteMax

getBits :: (Bits a, Num a) => Int -> Int -> a -> a
getBits start end x = (x .&. (1 `shiftL` start - 1)) `shiftR` end

-- everything around lengthInBytes should be simplified

lengthInBytes :: FiniteBits b => b -> Int
lengthInBytes x = 1 + logBase2 x `shiftR` 3

logBase2 :: FiniteBits b => b -> Int
logBase2 x = finiteBitSize x - 1 - countLeadingZeros x

putInt :: Int -> Put
putInt n | n < neg2ByteMin = do
  let l = lengthInBytes (complement n)
  let wordSize = 8 -- this should not be hardcoded
  putWord8 $ negMultiMarker .|. (wordSize - fromIntegral l)
  forM_ [8*(l-1), 8*(l-2) .. 0] $ \s ->
    putWord8 $ fromIntegral (n `shiftR` s)

putInt n | n < neg1ByteMin = do
  let x = n - fromIntegral neg2ByteMin
  putWord8 $ neg2ByteMarker .|. fromIntegral (getBits 13 8 x)
  putWord8 $ fromIntegral (getBits 8 0 x)

putInt n | n < 0 = do
  let x = n - fromIntegral neg1ByteMin
  putWord8 $ neg1ByteMarker .|. fromIntegral (getBits 6 0 x)

putInt n = putWord (fromIntegral n)

putWord :: Word -> Put
putWord n | n <= pos1ByteMax =
  putWord8 $ pos1ByteMarker .|. fromIntegral (getBits 6 0 n)

putWord n | n <= pos2ByteMax = do
  let x = n - pos1ByteMax - 1
  putWord8 $ pos2ByteMarker .|. fromIntegral (getBits 13 8 x)
  putWord8 $ fromIntegral (getBits 8 0 x)

putWord n | n == pos2ByteMax + 1 = do
  putWord8 $ posMultiMarker .|. 0x01
  putWord8 0

putWord n = do
  let x = n - pos2ByteMax - 1
  let l = lengthInBytes x
  putWord8 $ posMultiMarker .|. fromIntegral l
  forM_ [8*(l-1), 8*(l-2) .. 0] $ \s ->
    putWord8 $ fromIntegral (x `shiftR` s)

getWord :: Get Word
getWord = do
  x <- lookAhead getWord8
  let m = x .&. 0xf0
  if | m == pos1ByteMarker          -> get1Byte
     | m == pos1ByteMarker .|. 0x10 -> get1Byte
     | m == pos1ByteMarker .|. 0x20 -> get1Byte
     | m == pos1ByteMarker .|. 0x30 -> get1Byte
     | m == pos2ByteMarker          -> get2Byte
     | m == pos2ByteMarker .|. 0x10 -> get2Byte
     | otherwise                    -> getMulti

  where

    get1Byte = getBits 6 0 . fromIntegral <$> getWord8

    get2Byte = do
      hi <- getBits 5 0 . fromIntegral <$> getWord8
      lo <- fromIntegral <$> getWord8
      let x = hi `shiftL` 8 .|. lo
      return $ pos1ByteMax + 1 + x

    getMulti = do
      l <- (.&. 0xf) . fromIntegral <$> getWord8
      rs <- replicateM l (fromIntegral <$> getWord8)
      let r = foldl (\a x -> a `shiftL` 8 .|. x) 0 rs
      return $ 1 + pos2ByteMax + r

getInt :: Get Int
getInt = do
  x <- lookAhead getWord8
  let m = x .&. 0xf0
  if | m == negMultiMarker          -> getMulti
     | m == neg2ByteMarker          -> get2Byte
     | m == neg2ByteMarker .|. 0x10 -> get2Byte
     | m == neg1ByteMarker          -> get1Byte
     | m == neg1ByteMarker .|. 0x10 -> get1Byte
     | m == neg1ByteMarker .|. 0x20 -> get1Byte
     | m == neg1ByteMarker .|. 0x30 -> get1Byte
     | otherwise                    -> fromIntegral <$> getWord

  where

    getMulti = do
      l <- (8-) . (.&. 0xf) . fromIntegral <$> getWord8
      rs <- replicateM l (fromIntegral <$> getWord8)
      return $ foldl (\a x -> a `shiftL` 8 .|. x) maxBound rs

    get2Byte = do
      hi <- fromIntegral . getBits 5 0 <$> getWord8
      lo <- fromIntegral <$> getWord8
      return $ neg2ByteMin + (hi `shiftL` 8 .|. lo)

    get1Byte = (+ neg1ByteMin) . fromIntegral . getBits 6 0 <$> getWord8


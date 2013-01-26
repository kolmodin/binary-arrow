{-# LANGUAGE BangPatterns #-}

-- TODO: We can sometimes read whole Word16, Word32, Word64 using peekByteOff.
-- Need to use CPP to do it for the right endian, and only on the arches that supports unaligned reads.
-- Seems to give ~10% speedup on my macbook (core i5).

module Data.Binary.Get.Arrow.Decoder
  ( string
  , word8
  , word16be
  , word16le
  , word24be
  , word24le
  , word32be
  , word32le
  , word64be
  , word64le
  , int8
  , int16be
  , int16le
  , int32be
  , int32le
  , int64be
  , int64le
  ) where

import Data.Binary.Get.Arrow.Core

import Data.Word
import Data.Int
import Data.Bits

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

import Foreign.ForeignPtr       (withForeignPtr)

import Foreign.Storable         (Storable(..))
import Foreign.Ptr              (castPtr)

string :: Int -> GetA () B.ByteString
string !n = S n (\s _ -> B.unsafeTake n s)
{-# INLINE string #-}

word8 :: GetA () Word8
word8 = S 1 (const . B.unsafeHead)
{-# INLINE word8 #-}

word16be :: GetA () Word16
word16be = S 2 (const . readWord16be)
{-# INLINE word16be #-}

word16le :: GetA () Word16
word16le = S 2 (const . readWord16le)
{-# INLINE word16le #-}

word24be :: GetA () Word32
word24be = S 3 (const . readWord24be)
{-# INLINE word24be #-}

word24le :: GetA () Word32
word24le = S 3 (const . readWord24le)
{-# INLINE word24le #-}

word32be :: GetA () Word32
word32be = S 4 (const . readWord32be)
{-# INLINE word32be #-}

word32le :: GetA () Word32
word32le = S 4 (const . readWord32le)
{-# INLINE word32le #-}

word64be :: GetA () Word64
word64be = S 8 (const . readWord64be)
{-# INLINE word64be #-}

word64le :: GetA () Word64
word64le = S 8 (const . readWord64le)
{-# INLINE word64le #-}

int8 :: GetA () Int8
int8 = fmap fromIntegral word8
{-# INLINE int8 #-}

int16le :: GetA () Int16
int16le = fmap fromIntegral word16le
{-# INLINE int16le #-}

int16be :: GetA () Int16
int16be = fmap fromIntegral word16be
{-# INLINE int16be #-}

int32le :: GetA () Int32
int32le = fmap fromIntegral word32le
{-# INLINE int32le #-}

int32be :: GetA () Int32
int32be = fmap fromIntegral word32be
{-# INLINE int32be #-}

int64le :: GetA () Int64
int64le = fmap fromIntegral word64le
{-# INLINE int64le #-}

int64be :: GetA () Int64
int64be = fmap fromIntegral word64be
{-# INLINE int64be #-}

readWord16be :: B.ByteString -> Word16
readWord16be = \s ->
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 8) .|.
  (fromIntegral (s `B.unsafeIndex` 1))

readWord16le :: B.ByteString -> Word16
readWord16le = \s ->
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 8) .|.
  (fromIntegral (s `B.unsafeIndex` 0) )

readWord24be :: B.ByteString -> Word32
readWord24be = \s ->
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 16) .|.
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL`  8) .|.
  (fromIntegral (s `B.unsafeIndex` 2) )

readWord24le :: B.ByteString -> Word32
readWord24le = \s ->
  (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 16) .|.
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL`  8) .|.
  (fromIntegral (s `B.unsafeIndex` 0) )

readWord32be :: B.ByteString -> Word32
readWord32be = \s ->
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 24) .|.
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 16) .|.
  (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL`  8) .|.
  (fromIntegral (s `B.unsafeIndex` 3) )

readWord32le :: B.ByteString -> Word32
readWord32le (B.PS x s _l) =
   B.inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff (castPtr p) s
{-
readWord32le = \s ->
  (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 24) .|.
  (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 16) .|.
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL`  8) .|.
  (fromIntegral (s `B.unsafeIndex` 0) )
-}

readWord64be :: B.ByteString -> Word64
readWord64be = \s ->
  (fromIntegral (s `B.unsafeIndex` 0) `unsafeShiftL` 56) .|.
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL` 48) .|.
  (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 40) .|.
  (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 32) .|.
  (fromIntegral (s `B.unsafeIndex` 4) `unsafeShiftL` 24) .|.
  (fromIntegral (s `B.unsafeIndex` 5) `unsafeShiftL` 16) .|.
  (fromIntegral (s `B.unsafeIndex` 6) `unsafeShiftL`  8) .|.
  (fromIntegral (s `B.unsafeIndex` 7) )

readWord64le :: B.ByteString -> Word64
readWord64le = \s ->
  (fromIntegral (s `B.unsafeIndex` 7) `unsafeShiftL` 56) .|.
  (fromIntegral (s `B.unsafeIndex` 6) `unsafeShiftL` 48) .|.
  (fromIntegral (s `B.unsafeIndex` 5) `unsafeShiftL` 40) .|.
  (fromIntegral (s `B.unsafeIndex` 4) `unsafeShiftL` 32) .|.
  (fromIntegral (s `B.unsafeIndex` 3) `unsafeShiftL` 24) .|.
  (fromIntegral (s `B.unsafeIndex` 2) `unsafeShiftL` 16) .|.
  (fromIntegral (s `B.unsafeIndex` 1) `unsafeShiftL`  8) .|.
  (fromIntegral (s `B.unsafeIndex` 0) )

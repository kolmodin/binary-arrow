{-# LANGUAGE BangPatterns #-}

-- |
-- Module      :  Data.Binary.Get.Arrow.VarSize
-- Copyright   :  2013 Google Inc.
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
module Data.Binary.Get.Arrow.VarSize
  ( varInt32
  ) where

import Data.Binary.Get.Arrow.Core

import Control.Applicative
import Control.Arrow
import Control.Category
import Data.Int
import Data.Word
import Data.Bits

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

data Decode a = NotEnoughInput
            | InvalidData
            | HereItIs {-# UNPACK #-} !Int !a
            deriving Show

-- | Decode a base 128 variable size 'Int32'.
-- Currently, it treats the Ints as unsigned.
-- <https://developers.google.com/protocol-buffers/docs/encoding#varints>
varInt32 :: GetA () Int32
varInt32 = varsize 1 (varBytes (0::Int32)) varInt
{-# INLINE varInt32 #-}

varsize :: Int -> Int -> (B.ByteString -> Decode a) -> GetA () a
varsize s_min s_max fast
  | s_min == s_max = static s_min (\s _ -> case fast s of HereItIs _ x -> x)
  | otherwise =
      dynamic s_min (\s _ -> case fast s of
                   HereItIs n x -> SP (B.unsafeDrop n s) (pure x)
                   InvalidData -> SP s (pure undefined)
                   NotEnoughInput -> SP s (varsize2 (s_min+1) s_max fast))
{-# INLINE varsize #-}

varsize2 :: Int -> Int -> (B.ByteString -> Decode a) -> GetA () a
varsize2 s_min s_max fast
  | s_min == s_max = static s_min (\s _ -> case fast s of HereItIs _ x -> x)
  | otherwise =
      dynamic s_min (\s _ -> case fast s of
                   HereItIs n x -> SP (B.unsafeDrop n s) (pure x)
                   InvalidData -> SP s (pure undefined)
                   NotEnoughInput -> SP s (varsize2 (s_min+1) s_max fast))

varBytes :: (Integral b, Bits a) => a -> b
varBytes x = ceiling (fromIntegral (bitSize x) / 7)
{-# INLINE varBytes #-}

{-# SPECIALIZE varInt :: B.ByteString -> Decode Int8 #-}
{-# SPECIALIZE varInt :: B.ByteString -> Decode Int16 #-}
{-# SPECIALIZE varInt :: B.ByteString -> Decode Int32 #-}
{-# SPECIALIZE varInt :: B.ByteString -> Decode Int64 #-}
{-# INLINE varInt #-}
varInt :: (Num a, Bits a) => B.ByteString -> Decode a
varInt str = go acc0 0 0 (B.take bytes str)
  where
    !acc0 = 0
    !bytes = ceiling (fromIntegral (bitSize acc0) / 7)
    go !acc !pos !i !str
      | B.length str <= i = if i == bytes
                              then InvalidData
                              else NotEnoughInput
      | otherwise = 
          let !byte = B.unsafeIndex str i
              !continue = byte >= 128
              !val = fromIntegral (byte .&. 0x7F)
              !acc' = (val `unsafeShiftL` pos) .|. acc
              !i' = i+1
              !pos' = pos + 7
          in if continue
               then go acc' pos' i' str
               else HereItIs i' acc'

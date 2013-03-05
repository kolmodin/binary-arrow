
-- |
-- Module      :  Data.Binary.Get.Arrow.Vector
-- Copyright   :  2013 Google Inc.
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
module Data.Binary.Get.Arrow.Vector where

import Data.Binary.Get.Arrow.Core

import qualified Data.ByteString.Unsafe as B

import Data.Vector as V
import Data.Vector.Unboxed as U

toUnboxedVector :: U.Unbox a => Int -> GetA () a -> GetA () (U.Vector a)
toUnboxedVector _ (F str) = F str
toUnboxedVector reps (S n f) = S (reps*n) $ \ bs _ ->
  U.generate reps (\i -> f (B.unsafeDrop (i*n) bs) ())
{-# INLINE toUnboxedVector #-}

toVector :: Int -> GetA () a -> GetA () (V.Vector a)
toVector _ (F str) = F str
toVector reps (S n f) = S (reps*n) $ \ bs _ ->
  V.generate reps (\i -> f (B.unsafeDrop (i*n) bs) ())
{-# INLINE toVector #-}

-- |
-- Module      :  Data.Binary.Get.Arrow.Compat
-- Copyright   :  2013 Google Inc.
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC

module Data.Binary.Get.Arrow.Compat where

import qualified Data.ByteString as B
import qualified Data.Binary.Get as Bin
import qualified Data.Binary.Get.Internal as Bin

import Data.Binary.Get.Arrow.Core ( GetA(..), SP(..) )

runArrow :: GetA () a -> Bin.Get a
runArrow (S n f) = Bin.readN n (\bs -> f bs ())
runArrow (F str) = fail str
runArrow (D _ n f) = do
  bs <- Bin.getByteString n
  let SP bs' f' = f bs ()
  pushBack bs'
  runArrow f'
  where
    pushBack bs
      | B.null bs = return ()
      | otherwise = do
          bs0 <- Bin.get
          Bin.put (B.append bs bs0)

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Get.Arrow
-- Copyright   :  2013 Google Inc.
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
-- Decoding of binary values using Arrows <http://www.haskell.org/arrows/>.
-- 
-- The arrow is called 'GetA'. Here's one way how to parse two 'Word32's
-- in little endian, in a syntax that should be familiar to those who use
-- monads.
-- 
-- @
--    readTwoWord32s :: 'GetA' () ('Word32','Word32')
--    readTwoWord32s = proc _ -> do
--        w0 <- 'word32le' -< ()
--        w1 <- 'word32le' -< ()
--        'returnA' -< (w0, w1)
-- @
-- 
-- Let's inspect the @readTwoWord32s@ in @GHCi@:
-- 
-- >>> readTwoWord32s
-- <Static 8>
--
-- The decoder knows it will need to consume 8 bytes before returning an answer.
-- When running the decoder, it will make sure that there is enough input before
-- the decoder even starts. Once all the input is available the decoder will
-- execute and it can do so efficiently without any further checks to see
-- whether there is sufficient input left or not.
--
-- Naturally, it's not always the case that the size of the required input
-- can determined statically, it can depend on the input itself and
-- therefore be different for each time.
-- 
-- Here's how we parse a size prefixed 'Data.ByteString.ByteString':
--
-- @
--    sizePrefixedByteString :: 'GetA' () 'Data.ByteString.ByteString'
--    sizePrefixedByteString = proc _ -> do
--        -- size of string is encoded into a little endian 4 byte word
--        size <- 'word32le' -< ()
--        'string' ('fromIntegral' size) -<< ()
-- @
--
-- The @-<<@ arrow allows us to use the parsed length of the string as an
-- argument to the 'string' decoder that will read a string of that length.
-- If we would use the @-<@ arrow as we usually would, any value from an
-- executed decoder cannot be used as input to another decoder, or we would
-- not be able to statically determin the required input size of the parser.
-- 
-- So what about our size prefixed 'Data.ByteString.ByteString' decoder? Let's
-- have a look in GHCi:
--
-- >>> sizePrefixedByteString
-- <Dynamic 4>
--
-- This means that the computation is dynamic (depending on its input),
-- but it will at least require 4 bytes. Internally, a new decoder
-- (static or dynamic) will be created and executed.
-- This process will be repeated until it either can produce an answer,
-- or it runs out of input.
--
module Data.Binary.Get.Arrow
  (
  -- * The decoder arrow
    GetA
  -- ** Tracking progress of a decoder
  , Decoder(..)

  -- ** Running a decoder
  , run
  , runSimple

  -- ** Failing a decoder
  , failA

  -- * Primitive decoders
  -- ** ByteString
  , string
  -- ** Words and Ints
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

  -- ** Base 128 variable size Ints
  , varInt32

  -- * Combinators
  , staticLookAhead
  , lookAhead
  , list
  , isolate
  , unless
  , when
  , some
  , many
  , Comb.foldr
  , Comb.foldl'

  -- * Runtime information - experimental!

  , RuntimeInfo(..)
  , runtimeInfo

  -- * Utilities
  , atrace

  
  ) where

import Data.Binary.Get.Arrow.Core
import Data.Binary.Get.Arrow.Decoder
import Data.Binary.Get.Arrow.Combinator as Comb
import Data.Binary.Get.Arrow.VarSize

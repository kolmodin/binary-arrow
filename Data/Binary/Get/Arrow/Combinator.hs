{-# LANGUAGE Arrows, BangPatterns #-}

-- |
-- Module      :  Data.Binary.Get.Arrow.Combinator
-- Copyright   :  2013 Google Inc.
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
module Data.Binary.Get.Arrow.Combinator
  ( staticLookAhead
  , lookAhead
  , when
  , unless
  , list
  , isolate
  , some
  , many
  , stringUntil
  , Data.Binary.Get.Arrow.Combinator.foldr
  , Data.Binary.Get.Arrow.Combinator.foldl'
  ) where

import Data.Binary.Get.Arrow.Core

import Control.Applicative hiding (many,some)
import Control.Arrow

import qualified Data.List (foldl')

import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

-- | Lookahead for when the size is statically known. If the given decoder
-- fails, it'll fail the main computation as well.
staticLookAhead :: Int -> GetA a b -> GetA a b
staticLookAhead n _ | n < 0 =
  F "Binary.staticLookAhead: called with negative argument"
staticLookAhead _ (F str) = F str
staticLookAhead limit (S n f)
  | n > limit = F "Binary.staticLookAhead: static decoder requested too much input"
  | otherwise = lookAhead (S n f)
staticLookAhead limit a@(D _ _) =
  D limit $ \s x ->
    SP s $ case runChunk (pure x >>> a) s of
    	       Done y -> pure y
    	       Fail str -> F str
    	       NeedMoreInput _ _ -> F "Binary.staticLookAhead: dynamic decoder requested too much input"

-- | Look ahead when the size is unknown. If the given decoder
-- fails, it'll fail the main computation as well.
lookAhead :: GetA a b -> GetA a b
lookAhead (F str) = F str
lookAhead (S n f) = D n $ \s x -> SP s (pure (f s x))
lookAhead a@(D _ _) = proc x -> do
  rti <- runtimeInfo -< ()
  ei <- rtoa (runAndKeepTrack' rti (pure x >>> a)) -<< ()
  case ei of
    (Done y, saved) -> do
      pushBack -< saved
      returnA -< y
    (Fail str, _saved) -> failA str -<< ()
    (NeedMoreInput _ _, _saved) -> error "Binary: impossible" -< ()

when :: Bool -> GetA a () -> GetA a ()
when c ifTrue = proc x -> do
  if c
    then ifTrue -< x
    else returnA -< ()
{-# INLINE when #-}

unless :: Bool -> GetA a () -> GetA a ()
unless c ifFalse = when (not c) ifFalse
{-# INLINE unless #-}

list :: Int -> GetA () a -> GetA () [a]
list reps a
  | reps <= 0 = proc _ -> returnA -< []
  | otherwise =
      case a of
        F str -> F str
        S n f -> S (reps*n) (\s _ -> [ f (B.unsafeDrop (n*i) s) () | i <- [0..reps-1]])
        D n f -> list_dynamic reps n f
{-# INLINE list #-}

-- list_dynamic :: Int -> GetA () a -> GetA () [a]
list_dynamic 0 _ _ = arr (\_ -> [])
list_dynamic reps n f = proc _ -> do
  x <- (D n f) -< ()
  xs <- list_dynamic (reps - 1) n f -< ()
  returnA -< x:xs

isolate :: Int -> GetA a b -> GetA a b
isolate limit a
  | limit < 0 = F "Binary.isolate: given negative argument"
  | otherwise =
      case a of
        F str -> F str
        S n f | n == limit -> S n f
              | otherwise -> F "Binary.isolate: decoder requested too little/much input"
        D n f | n > limit -> F "Binary.isolate: dynamic decoder requsted too much input"
              | otherwise -> D n $ \s x -> -- TODO: isolate could request 'limit' bytes right away for D.
                 let SP s' a' = f (B.take limit s) x
                     used = B.length s - B.length s'
                 in SP s' (isolate (limit - used) a')
{-# INLINE isolate #-}

-- TODO: 'some', 'many' makes sense?
some :: GetA () (Maybe b) -> GetA () [b]
some a = proc _ -> do
  vm <- a -< ()
  case vm of
    Nothing -> F "Binary.some: could not parse" -< ()
    Just v -> do
      vs <- many a -< ()
      returnA -< v:vs

many :: GetA () (Maybe b) -> GetA () [b]
many a = proc _ -> do
  vm <- a -< ()
  case vm of
    Nothing -> returnA -< []
    Just v -> do
      vs <- many a -< ()
      returnA -< v:vs

stringUntil :: Word8 -> GetA () B.ByteString
stringUntil w8 = D 1 $ \s0 _ ->
  case B.elemIndex w8 s0 of
  	Just i -> SP (B.unsafeDrop i s0) (pure (B.unsafeTake i s0))
  	Nothing ->
  	  SP B.empty $ proc _ -> do
  	  	xs <- stringUntil w8 -< () -- TODO: O(n^2)
  	  	returnA -< B.append s0 xs

foldr :: Int -> (a -> b -> b) -> b -> GetA () a -> GetA () b
foldr n f i a =
  case a of
    -- TODO: foldr does not implement D, I, F
    S m g -> S (n*m) (\s _ -> Prelude.foldr f i [ g (B.unsafeDrop (m*i) s) () | i <- [0..n-1]])
    _ -> error "Binary.foldr: not implemented"
{-# INLINE foldr #-}

foldl' :: Int -> (a -> b -> a) -> a -> GetA () b -> GetA () a
foldl' n0 f i a =
  case a of
    -- TODO: foldl' does not implement D, I, F
    
    -- Note: GHC doesn't fuse foldl' and the list comprehension, so we do it ourselves.
    --     S m g -> S (n0*m) (\s _ -> Data.List.foldl' f i [ g (B.unsafeDrop (m*i) s) () | i <- [0..n0-1]])
    S m g -> S (n0*m) (\s _ -> goS i m s g)
    _ -> error "Binary.foldl': not implemented"
  where
    goS !firstValue0 !m !s0 !g = goS' firstValue0 (B.unsafeTake (n0*m) s0)
      where
        goS' !firstValue !s
          | B.null s = firstValue
          | otherwise = goS' (f firstValue (g s ())) (B.unsafeDrop m s)
{-# INLINE foldl' #-}
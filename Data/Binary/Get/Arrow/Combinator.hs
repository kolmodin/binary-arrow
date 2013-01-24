{-# LANGUAGE Arrows #-}
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
  ) where

import Data.Binary.Get.Arrow.Core

import Control.Applicative hiding (many,some)
import Control.Arrow

import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

-- | Lookahead for when the size is statically known.
staticLookAhead :: Int -> A a b -> A a b
staticLookAhead n _ | n < 0 =
  F "Binary: staticLookAhead called with negative argument"
staticLookAhead _ (F str) = F str
staticLookAhead limit (S n f)
  | n > limit = F "Binary.staticLookAhead: static decoder requested too much input"
  | otherwise = lookAhead (S n f)
staticLookAhead limit a@(D _ _) =
  D limit $ \s x ->
    SP s $ case runChunk (pure x >>> a) s of
    	       Done y -> pure y
    	       Fail str -> F str
    	       Hungry _ _ -> F "Binary.staticLookAhead: dynamic decoder requested too much input"

-- | Lookahead for when the size is unknown.
lookAhead :: A a b -> A a b
lookAhead (F str) = F str
lookAhead (S n f) = D n $ \s x -> SP s (pure (f s x))
lookAhead a@(D _ _) = proc x -> do
  ei <- rtoa (runAndKeepTrack' (pure x >>> a)) -<< ()
  case ei of
    (Done y, saved) -> do
      pushBack -< saved
      returnA -< y
    (Fail str, _saved) -> failA str -<< ()
    (Hungry _ _, _saved) -> error "Binary: impossible" -< ()

when :: Bool -> A a () -> A a ()
when c ifTrue = proc x -> do
  if c
    then ifTrue -< x
    else returnA -< ()
{-# INLINE when #-}

unless :: Bool -> A a () -> A a ()
unless c ifFalse = when (not c) ifFalse
{-# INLINE unless #-}

list :: Int -> A () a -> A () [a]
list reps a
  | reps <= 0 = proc _ -> returnA -< []
  | otherwise =
      case a of
        F str -> F str
        S n f -> S (reps*n) (\s _ -> [ f (B.unsafeDrop (n*i) s) () | i <- [0..reps-1]])
        D n f -> list_dynamic reps n f
{-# INLINE list #-}

-- list_dynamic :: Int -> A () a -> A () [a]
list_dynamic reps n f = proc _ -> do
  x <- (D n f) -< ()
  xs <- list_dynamic (reps - 1) n f -< ()
  returnA -< x:xs

isolate :: Int -> A a b -> A a b
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
some :: A () (Maybe b) -> A () [b]
some a = proc _ -> do
  vm <- a -< ()
  case vm of
    Nothing -> F "Binary.some: could not parse" -< ()
    Just v -> do
      vs <- many a -< ()
      returnA -< v:vs

many :: A () (Maybe b) -> A () [b]
many a = proc _ -> do
  vm <- a -< ()
  case vm of
    Nothing -> returnA -< []
    Just v -> do
      vs <- many a -< ()
      returnA -< v:vs

stringUntil :: Word8 -> A () B.ByteString
stringUntil w8 = D 1 $ \s0 _ ->
  case B.elemIndex w8 s0 of
  	Just i -> SP (B.unsafeDrop i s0) (pure (B.unsafeTake i s0))
  	Nothing ->
  	  SP B.empty $ proc _ -> do
  	  	xs <- stringUntil w8 -< () -- TODO: O(n^2)
  	  	returnA -< B.append s0 xs

foldr :: Int -> (a -> b -> b) -> b -> A () a -> A () b
foldr n f i a =
  case a of
    -- TODO: foldr does not implement D, I, F
    S m g -> S (n*m) (\s _ -> Prelude.foldr f i [ g (B.unsafeDrop (m*i) s) () | i <- [0..n-1]])
    _ -> error "Binary.foldr: not implemented"
{-# INLINE foldr #-}
{-# LANGUAGE Arrows, TupleSections, BangPatterns, RankNTypes #-}

-- |
-- Module      :  Data.Binary.Get.Arrow.Core
-- Copyright   :  2013 Google Inc.
-- License     :  Apache-2.0 (see the LICENSE file in the distribution)
--
-- Maintainer  :  Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   :  experimental
-- Portability :  GHC
--
module Data.Binary.Get.Arrow.Core
  ( GetA(..)
  , SP(..)
  , Decoder(..)

  , dynamic
  , static

  , run
  , runChunk
  , runAndKeepTrack
  , runAndKeepTrack'
  , failA
  , rtoa
  , pushBack
  , runSimple
  , atrace
  )
  where

import Control.Applicative
import Control.Arrow
import Control.Category

import Prelude hiding ((.), id)

import Debug.Trace

import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B

import GHC.Exts ( inline )

data GetA a b
  = S {-# UNPACK #-} !Int !(B.ByteString -> a -> b)
  | D () {-# UNPACK #-} !Int !(B.ByteString -> a -> SP B.ByteString (GetA () b))
  | F String

-- Idea for position awareness:
-- Let the continuation in D take the position parameter.

-- Idea for "more input available" awareness:
-- Add boolean parameter to D.
-- If True, behaviour is as normal.
-- If False, a shorter ByteString than requested can be proveded
-- (although not less than D's parameter). The continuation can check the
-- length of the input and figure out if there is more input or not.
-- If there is more input, strictly more data than requested must be provided.

data SP a b = SP !a !b
  deriving Show

dynamic :: Int -> (B.ByteString -> a -> SP B.ByteString (GetA () b)) -> GetA a b
dynamic = D ()

static :: Int -> (B.ByteString -> a -> b) -> GetA a b
static = S

instance Functor (GetA a) where
  fmap = fmap' ()
  {-# INLINE fmap #-}

fmap_cont :: (forall xa xb xc. () -> (xb -> xc) -> GetA xa xb -> GetA xa xc)
          -> (b -> c) -> GetA a b -> GetA a c
fmap_cont _    f (S n g) = static n (\s b -> f (g s b))
fmap_cont cont f (D u n g) = dynamic n (\s b -> let SP s' c = g s b in SP s' (cont u f c))
fmap_cont _    _ (F str) = F str

fmap' :: () -> (b -> c) -> GetA a b -> GetA a c
fmap' _ = fmap_cont fmap'
{-# NOINLINE fmap' #-}

{-# RULES
"GetA/fmap" fmap' () = fmap_cont fmap'
 #-}

instance Show (GetA a b) where
  show (S n _) = "<Static " ++ show n ++ ">"
  show (D _ n _) = "<Dynamic " ++ show n ++ ">"
  show (F str) = "<Fail: " ++ show str ++ ">"

instance Category GetA where
  id = static 0 (\_ x -> x)
  {-# INLINE id #-}
  (.) = merge' ()
  {-# INLINE (.) #-}

merge_cont :: (forall xa xb xc. () -> GetA xb xc -> GetA xa xb -> GetA xa xc)
           -> GetA b c -> GetA a b -> GetA a c
merge_cont _    (S n f) (S m g) = static (n+m) (\s x -> f (B.unsafeDrop m s) (g s x))
merge_cont cont (S n f) (D u m g) = dynamic m (\s a -> let SP s' g' = g s a in SP s' (cont u (static n f) g'))
merge_cont cont (D _ n f) (D u m g) = dynamic m (\s a -> let SP s' g' = g s a in SP s' (cont u (dynamic n f) g'))
merge_cont _    (D _ n f) (S m g) = dynamic (n+m) (\s a -> f (B.unsafeDrop m s) (g s a))
merge_cont _    _ (F str) = F str
merge_cont _   (F str) (S _ _) = F str
merge_cont cont (F str) (D u n f) = dynamic n (\s x -> let SP s' f' = f s x in SP s' (cont u (F str) f'))

merge' :: () -> GetA b c -> GetA a b -> GetA a c
merge' _ = merge_cont merge'
{-# NOINLINE merge' #-}

{-# RULES
"GetA/merge" merge' () = merge_cont merge'
 #-}

instance Arrow GetA where
  arr f = static 0 (\_s a -> f a)
  {-# INLINE arr #-}
  first (S n f) = static n (\s (a,b) -> (f s a, b))
  first (D _ n f) = dynamic n (\s (a,b) -> let SP s' a' = f s a in SP s' (fmap (,b) a'))
  first (F str) = F str
  {-# INLINE first #-}

instance ArrowApply GetA where
  app = dynamic 0 (\s (a,i) -> SP s (pure i >>> a))
  {-# INLINE app #-}

instance ArrowZero GetA where
  zeroArrow = F "Binary: zeroArrow"

instance ArrowPlus GetA where
        (<+>) = plus

instance ArrowChoice GetA where
  left a = case a of
           F str -> F str
           S n f -> static n (\s b -> case b of
                                   Left lft -> Left (f s lft)
                                   Right rght -> Right rght)
           D _ n f -> dynamic n (\s x -> case x of
                                   Left lft -> let SP s' y = f s lft in SP s' (fmap Left y)
                                   Right rght -> SP s (pure (Right rght)))
  {-# INLINE left #-}
  f +++ g =
    case (f,g) of
        (S n h, S m k) | n == m -> static n (\s a -> case a of
                                                Left lft -> Left (h s lft)
                                                Right rght -> Right (k s rght))
{- Becomes huge expressions when inlined. Is it worth it?
        (D n h, D m k) | n == m -> D n (\s a -> case a of
                                                Left lft -> let (SP s' a') = h s lft in SP s' (a' >>> arr Left)
                                                Right rght -> let (SP s' a') = k s rght in SP s' (a' >>> arr Right))
        (D n h, S m k) | n == m -> D n (\s a -> case a of
                                                Left lft -> let (SP s' a') = h s lft in SP s' (a' >>> arr Left)
                                                Right rght -> SP s (arr (const rght) >>> S n k >>> arr Right))
        (S n h, D m k) | n == m -> D n (\s a -> case a of
                                                Left lft -> SP s (arr (const lft) >>> S n h >>> arr Left)
                                                Right rght -> let (SP s' a') = k s rght in SP s' (a' >>> arr Right))
-}
        _ -> dynamic 0 (\s a -> case a of
                              Left lft -> SP s (pure lft >>> f >>> arr Left)
                              Right rght -> SP s (pure rght >>> g >>> arr Right))
  {-# INLINE (+++) #-}

  f ||| g = f +++ g >>> arr untag
      where
        untag (Left x) = x
        untag (Right y) = y
  -- standard implementation, but make sure it gets inlined...
  {-# INLINE (|||) #-}

instance Applicative (GetA a) where
	pure x = static 0 (\_ _ -> x)
	{-# INLINE pure #-}
	(<*>) af ag = proc x -> do
		f <- af -< x
		g <- ag -< x
		returnA -< f g
	{-# INLINE (<*>) #-}

plus :: GetA a b -> GetA a b -> GetA a b
plus (S n f) _ = static n f
plus (F _) plan_b = plan_b
-- Run the first decoder until it either succeeds or fails, but keep track of all the input it uses.
-- If it succeeds, just proceed and throw the saved input.
-- If it fails, use the saved input to try plan_b.
plus plan_a plan_b = proc x -> do
  ei <- rtoa (runAndKeepTrack (pure x >>> plan_a)) -<< ()
  case ei of
    Right result -> returnA -< result
    Left saved_input -> do
      pushBack -< saved_input
      plan_b -< x

atrace :: GetA String ()
atrace = static 0 (\_s x -> trace x ())

failA :: String -> GetA a b
failA str = F str
{-# INLINE failA #-}

pushBack :: GetA [B.ByteString] ()
pushBack = dynamic 0 (\s bs -> SP (B.concat (bs ++ [s])) (pure ()))
{-# INLINE pushBack #-}

rtoa :: Decoder a -> GetA () a
rtoa (Done a) = static 0 (\_ _ -> a)
rtoa (Fail str) = F str -- should never happen if used with runAndKeepTracks
rtoa (NeedMoreInput n f) = dynamic n (\s _ -> SP B.empty (rtoa (f s)))

runAndKeepTrack :: GetA () b -> Decoder (Either [B.ByteString] b)
runAndKeepTrack a0 = go (runContinue a0 B.empty) []
  where
    go a saved =
      case a of
        Done x -> Done (Right x)
        Fail _ -> Done (Left (reverse saved))
        NeedMoreInput n f -> NeedMoreInput n $ \s -> go (f s) (s:saved)

runAndKeepTrack' :: GetA () b -> Decoder (Decoder b, [B.ByteString])
runAndKeepTrack' a0 = go (runContinue a0 B.empty) []
  where
    go a saved =
      case a of
        Done x -> Done (Done x, saved)
        Fail str -> Done (Fail str, saved)
        NeedMoreInput n f -> NeedMoreInput n $ \s -> go (f s) (s:saved)

runSimple :: GetA () a -> B.ByteString -> a
runSimple !a s0 =
	case runChunk a s0 of
		Fail str -> error $ "Binary: Failed miserably: " ++ str
		Done x -> x
		NeedMoreInput _ _ -> error "Binary: requested more input than available"
{-# INLINE runSimple #-}

data Decoder a
  = Done a
  | Fail String
  | NeedMoreInput {-# UNPACK #-} !Int (B.ByteString -> Decoder a)

instance Show a => Show (Decoder a) where
  show (Done a) = "Done: " ++ show a
  show (Fail str) = "Fail: " ++ str
  show (NeedMoreInput n _) = "NeedMoreInput for at least " ++ show n ++ " more bytes"

run :: GetA () b -> Decoder b
run a = runChunk a B.empty
{-# INLINE run #-}

runChunk :: GetA () a -> B.ByteString -> Decoder a
runChunk a bs = runContinue a bs
{-# INLINE runChunk #-}

runContinue :: GetA () a -> B.ByteString -> Decoder a
runContinue !a0 s0 = -- trace (show a0) $
  case a0 of
    F str -> Fail str
    S n f | B.length s0 >= n -> Done (f s0 ())
          | otherwise -> NeedMoreInput (n - B.length s0) $ \s ->
              runContinue  a0 (B.append s0 s)
    D _ n f | B.length s0 >= n ->
              case f s0 () of
              	SP s' a' ->
              	  let _used = B.length s0 - B.length s'
              	  in runContinue a' s'
          | otherwise -> NeedMoreInput (n - B.length s0) $ \s ->
              runContinue a0 (B.append s0 s)

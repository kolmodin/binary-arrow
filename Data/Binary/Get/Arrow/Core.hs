{-# LANGUAGE Arrows, TupleSections, BangPatterns #-}

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

  , run
  , runChunk
  , runAndKeepTrack
  , runAndKeepTrack'
  , runtimeInfo
  , RuntimeInfo(..)
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
  | D {-# UNPACK #-} !Int !(B.ByteString -> a -> SP B.ByteString (GetA () b))
  | F String
  | I {-# UNPACK #-} !Int !(B.ByteString -> a -> GetA RuntimeInfo b)

data RuntimeInfo =
	RTI { rtiByteOffset :: ByteOffset
      , rtiMoreInputAvailable :: Bool
      } deriving Show

type ByteOffset = Int

data SP a b = SP !a !b
  deriving Show

fstSP :: SP t t -> t
fstSP (SP a _) = a

sndSP :: SP t t -> t
sndSP (SP _ b) = b

instance Functor (GetA a) where
  fmap f a =
    case a of
      S n g -> S n (\s b -> f (g s b))
      D n g -> D n (\s b -> let SP s' c = g s b in SP s' (fmap' f c))
      I n g -> I n (\s x -> fmap' f (g s x))
      F str -> F str
  {-# INLINE fmap #-}

fmap' :: (t1 -> b) -> GetA t t1 -> GetA t b
fmap' f a = case a of
             S n g -> S n (\s b -> f (g s b))
             D n g -> D n (\s b -> let SP s' c = g s b in SP s' (fmap'' f c))
             F str -> F str
{-# INLINE fmap' #-}

fmap'' :: (t1 -> b) -> GetA t t1 -> GetA t b
fmap'' f a = case a of
             S n g -> S n (\s b -> f (g s b))
             D n g -> D n (\s b -> let SP s' c = g s b in SP s' (fmap'' f c))
             F str -> F str
{-# INLINE fmap'' #-}

instance Show (GetA a b) where
  show (S n _) = "<Static " ++ show n ++ ">"
  show (D n _) = "<Dynamic " ++ show n ++ ">"
  show (F str) = "<Fail: " ++ show str ++ ">"
  show (I ib _) = "<Info " ++ show ib ++ ">"

instance Category GetA where
  id = S 0 (\_ x -> x)
  {-# INLINE id #-}
  (S n f) . (I ib g) = I ib (\s x -> merge (S n f) (g s x))
  (D n f) . (I ib g) = I ib (\s x -> merge (D n f) (g s x))
  (F str) . (I ib g) = I ib (\s x -> merge (F str) (g s x))
  (I ib f) . (S m g) = I (ib+m) (\s x -> f (B.unsafeDrop m s) (g s x))
  (I ib f) . (D m g) = D m (\s x -> let SP s' b = g s x in SP s' (merge (I ib f) b))
  (I ib f) . (I ib' g) = I ib' (\s x -> merge (I ib f) (g s x))
  (S n f) . (S m g) = S (n+m) (\s x -> f (B.unsafeDrop m s) (g s x))
  (S n f) . (D m g) = D m (\s a -> let SP s' g' = g s a in SP s' (merge (S n f) g'))
  (D n f) . (D m g) = D m (\s a -> let SP s' g' = g s a in SP s' (merge (D n f) g'))
  (D n f) . (S m g) = D (n+m) (\s a -> f (B.unsafeDrop m s) (g s a))
  _ . (F str) = F str
  (F str) . (S _ _) = F str
  (F str) . (D n f) = D n (\s x -> let SP s' f' = f s x in SP s' (F str . f'))
  {-# INLINE [1] (.) #-}

-- Ah... rewrite rules.
-- They don't play along at all.

{-# RULES
"ss/me" forall n f m g.
  (.) (S n f) (S m g) = S (n+m) (\s x -> f (B.unsafeDrop m s) (g s x))
"sd/me" forall n f m g.
    (.) (S n f) (D m g) = D m $ \s a -> let SP s' g' = g s a in SP s' (S n f . g')
"dd/me" forall n f m g.
    (.) (D n f) (D m g) = D (n+m) $ \s a -> let SP s' g' = g s a in SP s' (D n f . g')
"ds/me" forall n f m g.
    (.) (D n f) (S m g) = D (n+m) $ \s a -> f (B.unsafeDrop m s) (g s a)
 #-}

instance Arrow GetA where
  arr f = S 0 (\_s a -> f a)
  {-# INLINE arr #-}
  first (S n f) = S n (\s (a,b) -> (f s a, b))
  first (D n f) = D n (\s (a,b) -> let SP s' a' = f s a in SP s' (fmap (,b) a'))
  first (I ib f) = I ib (\s (a,b) -> f s a >>> arr (\x -> (x,b)))
  first (F str) = F str
  {-# INLINE first #-}

instance ArrowApply GetA where
  app = D 0 (\s (a,i) -> SP s (pure i >>> a))
  {-# INLINE app #-}

instance ArrowZero GetA where
  zeroArrow = F "Binary: zeroArrow"

instance ArrowPlus GetA where
        (<+>) = plus

instance ArrowChoice GetA where
  left a = case a of
           F str -> F str
           S n f -> S n (\s b -> case b of
                                   Left lft -> Left (f s lft)
                                   Right rght -> Right rght)
           D n f -> D n (\s x -> case x of
                                   Left lft -> let SP s' y = f s lft in SP s' (fmap Left y)
                                   Right rght -> SP s (pure (Right rght)))
  {-# INLINE left #-}
  f +++ g =
    case (f,g) of
        (S n h, S m k) | n == m -> S n (\s a -> case a of
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
        _ -> D 0 (\s a -> case a of
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
	pure x = S 0 (\_ _ -> x)
	{-# INLINE pure #-}
	(<*>) af ag = proc x -> do
		f <- af -< x
		g <- ag -< x
		returnA -< f g
	{-# INLINE (<*>) #-}

merge :: GetA b c -> GetA a b -> GetA a c
merge (S n f) (I ib g) = I ib (\s x -> merge' (S n f) (g s x))
merge (D n f) (I ib g) = I ib (\s x -> merge' (D n f) (g s x))
merge (F str) (I ib g) = I ib (\s x -> merge' (F str) (g s x))
merge (I ib f) (S m g) = I (ib+m) (\s x -> f (B.unsafeDrop m s) (g s x))
merge (I ib f) (D m g) = D m (\s x -> let SP s' b = g s x in SP s' (merge' (I ib f) b))
merge (S n f) (S m g) = S (n+m) (\s x -> f (B.unsafeDrop m s) (g s x))
merge (S n f) (D m g) = D m $ \s a -> let SP s' g' = g s a in SP s' (merge' (S n f) g')
merge (D n f) (D m g) = D m $ \s a -> let SP s' g' = g s a in SP s' (merge' (D n f) g')
merge (D n f) (S m g) = D (n+m) $ \s a -> f (B.unsafeDrop m s) (g s a)
merge _ (F str) = F str
merge (F str) (S _ _) = F str
merge (F str) (D n f) = D n (\s x -> let SP s' f' = f s x in SP s' (merge' (F str) f'))
{- INLINE merge #-}

merge' :: GetA b c -> GetA a b -> GetA a c
merge' (S n f) (I ib g) = I ib (\s x -> merge'' (S n f) (g s x))
merge' (D n f) (I ib g) = I ib (\s x -> merge'' (D n f) (g s x))
merge' (F str) (I ib g) = I ib (\s x -> merge'' (F str) (g s x))
merge' (I ib f) (S m g) = I (ib+m) (\s x -> f (B.unsafeDrop m s) (g s x))
merge' (I ib f) (D m g) = D m (\s x -> let SP s' b = g s x in SP s' (merge'' (I ib f) b))
merge' (S n f) (S m g) = S (n+m) (\s x -> f (B.unsafeDrop m s) (g s x))
merge' (S n f) (D m g) = D m $ \s a -> let SP s' g' = g s a in SP s' (merge' (S n f) g')
merge' (D n f) (D m g) = D m $ \s a -> let SP s' g' = g s a in SP s' (merge'' (D n f) g')
merge' (D n f) (S m g) = D (n+m) $ \s a -> f (B.unsafeDrop m s) (g s a)
merge' _ (F str) = F str
merge' (F str) (S _ _) = F str
merge' (F str) (D n f) = D n (\s x -> let SP s' f' = f s x in SP s' (merge' (F str) f'))

merge'' :: GetA b c -> GetA a b -> GetA a c
merge'' (S n f) (I ib g) = I ib (\s x -> merge'' (S n f) (g s x))
merge'' (D n f) (I ib g) = I ib (\s x -> merge'' (D n f) (g s x))
merge'' (F str) (I ib g) = I ib (\s x -> merge'' (F str) (g s x))
merge'' (I ib f) (S m g) = I (ib+m) (\s x -> f (B.unsafeDrop m s) (g s x))
merge'' (I ib f) (D m g) = D m (\s x -> let SP s' b = g s x in SP s' (merge'' (I ib f) b))
merge'' (S n f) (S m g) = S (n+m) (\s x -> f (B.unsafeDrop m s) (g s x))
merge'' (S n f) (D m g) = D m $ \s a -> let SP s' g' = g s a in SP s' (merge'' (S n f) g')
merge'' (D n f) (D m g) = D m $ \s a -> let SP s' g' = g s a in SP s' (merge'' (D n f) g')
merge'' (D n f) (S m g) = D (n+m) $ \s a -> f (B.unsafeDrop m s) (g s a)
merge'' _ (F str) = F str
merge'' (F str) (S _ _) = F str
merge'' (F str) (D n f) = D n (\s x -> let SP s' f' = f s x in SP s' (merge'' (F str) f'))

plus :: GetA a b -> GetA a b -> GetA a b
plus (S n f) _ = S n f
plus (F _) plan_b = plan_b
-- Run the first decoder until it either succeeds or fails, but keep track of all the input it uses.
-- If it succeeds, just proceed and throw the saved input.
-- If it fails, use the saved input to try plan_b.
plus plan_a plan_b = proc x -> do
  rti <- runtimeInfo -< ()
  ei <- rtoa (runAndKeepTrack rti (pure x >>> plan_a)) -<< ()
  case ei of
    Right result -> returnA -< result
    Left saved_input -> do
      pushBack -< saved_input
      plan_b -< x

atrace :: GetA String ()
atrace = S 0 (\_s x -> trace x ())

failA :: String -> GetA a b
failA str = F str
{-# INLINE failA #-}

pushBack :: GetA [B.ByteString] ()
pushBack = D 0 (\s bs -> SP (B.concat (bs ++ [s])) (pure ()))
{-# INLINE pushBack #-}

rtoa :: Decoder a -> GetA () a
rtoa (Done a) = S 0 (\_ _ -> a)
rtoa (Fail str) = F str -- should never happen if used with runAndKeepTracks
rtoa (NeedMoreInput n f) = D n (\s _ -> SP B.empty (rtoa (f s)))

runAndKeepTrack :: RuntimeInfo -> GetA () b -> Decoder (Either [B.ByteString] b)
runAndKeepTrack rti a0 = go (runContinue (rtiByteOffset rti) (rtiMoreInputAvailable rti) a0 B.empty) []
  where
    go a saved =
      case a of
        Done x -> Done (Right x)
        Fail _ -> Done (Left (reverse saved))
        NeedMoreInput n f -> NeedMoreInput n $ \s -> go (f s) (s:saved)

runAndKeepTrack' :: RuntimeInfo -> GetA () b -> Decoder (Decoder b, [B.ByteString])
runAndKeepTrack' rti a0 = go (runContinue (rtiByteOffset rti) (rtiMoreInputAvailable rti) a0 B.empty) []
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

runtimeInfo :: GetA () RuntimeInfo
runtimeInfo = I 0 (\_ _ -> S 0 (\_bs rti -> rti))
{-# INLINE runtimeInfo #-}

run :: GetA () b -> Decoder b
run a = runChunk a B.empty

runChunk :: GetA () a -> B.ByteString -> Decoder a
runChunk a bs = runContinue 0 True a bs
{-# INLINE runChunk #-}

runContinue :: Int -> Bool -> GetA () a -> B.ByteString -> Decoder a
runContinue pos haveMore !a0 s0 = -- trace (show a0) $
  case a0 of
    F str -> Fail str
    S n f | B.length s0 >= n -> Done (f s0 ())
          | otherwise -> NeedMoreInput (n - B.length s0) $ \s ->
              runContinue pos haveMore a0 (B.append s0 s)
    D n f | B.length s0 >= n ->
              case f s0 () of
              	SP s' a' ->
              	  let used = B.length s0 - B.length s'
              	  in runContinue (pos+used) haveMore a' s'
          | otherwise -> NeedMoreInput (n - B.length s0) $ \s ->
              runContinue pos haveMore a0 (B.append s0 s)
    I ib f | B.length s0 >= ib ->
                  let rti = RTI (pos+ib) haveMore
                  in runContinue (pos + ib) haveMore (pure rti >>> (f s0 ())) (B.unsafeDrop ib s0)
              | otherwise -> NeedMoreInput (ib - B.length s0) $ \s ->
                  runContinue pos haveMore a0 (B.append s0 s)

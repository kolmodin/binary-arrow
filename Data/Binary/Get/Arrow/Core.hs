{-# LANGUAGE Arrows, TupleSections, BangPatterns #-}

module Data.Binary.Get.Arrow.Core
  ( GetA(..)
  , SP(..)
  , R(..)

  , run
  , runChunk
  , runAndKeepTrack
  , runAndKeepTrack'
  , failA
  , runEither
  , rtoa
  , pushBack
  , runSimple
  , atrace
  , testRuntimeInfo
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
  | I !(a -> GetA RuntimeInfo b) -- TODO: 'I' should take BS and have Int requirement

data RuntimeInfo = RTI { rtiByteOffset :: ByteOffset
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
      I g -> I (\x -> fmap' f (g x))
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

instance Category GetA where
  id = S 0 (\_ x -> x)
  {-# INLINE id #-}
  (S n f) . (I a) = I (\x -> merge (S n f) (a x))
  (D n f) . (I a) = I (\x -> merge (D n f) (a x))
  (F str) . (I a) = I (\x -> merge (F str) (a x))
  (I a) . (S n f) = D n $ \s x -> SP (B.unsafeDrop n s) (I (\_ -> a (f s x)))
  (I a) . (D n f) = D n $ \s x -> let SP s' b = f s x in SP s' (merge (I a) b)
  --a . (I a) = I (merge (D n f) a)
  -- (D n f) . (I a) = D n (\s x -> let SP s' y = f s x in SP s' (merge (I a) y))
  (S n f) . (S m g) = S (n+m) (\s x -> f (B.unsafeDrop m s) (g s x))
  (S n f) . (D m g) = D m (\s a -> let SP s' g' = g s a in SP s' (merge (S n f) g'))
  (D n f) . (D m g) = D (n+m) (\s a -> let SP s' g' = g s a in SP s' (merge (D n f) g'))
  (D n f) . (S m g) = D (n+m) (\s a -> f (B.unsafeDrop m s) (g s a))
  _ . (F str) = F str
  (F str) . (S _ _) = F str
  (F str) . (D n f) = D n (\s x -> let SP s' f' = f s x in SP s' (F str . f'))
  {-# INLINE [1] (.) #-}

-- Ah... rewrite rules.
-- They don't play along at all.

{-# RULES

"si/me" forall n f a. merge (S n f) (I a) = I (\x -> merge' (S n f) (a x))

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
  first (D n f) = D n $ \s (a,b) -> let SP s' a' = f s a in SP s' (fmap (,b) a')
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
merge (S n f) (I a) = I (\x -> merge' (S n f) (a x))
merge (D n f) (I a) = I (\x -> merge' (D n f) (a x))
merge (F str) (I a) = I (\x -> merge' (F str) (a x))
merge (I a) (S n f) = D n $ \s x -> SP (B.unsafeDrop n s) (I (\_ -> a (f s x)))
merge (I a) (D n f) = D n $ \s x -> let SP s' b = f s x in SP s' (merge' (I a) b)
merge (S n f) (S m g) = S (n+m) (\s x -> f (B.unsafeDrop m s) (g s x))
merge (S n f) (D m g) = D m $ \s a -> let SP s' g' = g s a in SP s' (merge' (S n f) g')
merge (D n f) (D m g) = D (n+m) $ \s a -> let SP s' g' = g s a in SP s' (merge' (D n f) g')
merge (D n f) (S m g) = D (n+m) $ \s a -> f (B.unsafeDrop m s) (g s a)
merge _ (F str) = F str
merge (F str) (S _ _) = F str
merge (F str) (D n f) = D n (\s x -> let SP s' f' = f s x in SP s' (merge' (F str) f'))
{- INLINE merge #-}

merge' :: GetA b c -> GetA a b -> GetA a c
merge' (S n f) (I a) = I (\x -> merge'' (S n f) (a x))
merge' (D n f) (I a) = I (\x -> merge'' (D n f) (a x))
merge' (I a) (S n f) = D n $ \s x -> SP (B.unsafeDrop n s) (I (\_ -> a (f s x)))
merge' (I a) (D n f) = D n $ \s x -> let SP s' b = f s x in SP s' (merge' (I a) b)
merge' (S n f) (S m g) = S (n+m) (\s x -> f (B.unsafeDrop m s) (g s x))
merge' (S n f) (D m g) = D m $ \s a -> let SP s' g' = g s a in SP s' (merge' (S n f) g')
merge' (D n f) (D m g) = D (n+m) $ \s a -> let SP s' g' = g s a in SP s' (merge'' (D n f) g')
merge' (D n f) (S m g) = D (n+m) $ \s a -> f (B.unsafeDrop m s) (g s a)
merge' _ (F str) = F str
merge' (F str) (S _ _) = F str
merge' (F str) (D n f) = D n (\s x -> let SP s' f' = f s x in SP s' (merge' (F str) f'))

merge'' :: GetA b c -> GetA a b -> GetA a c
merge'' (S n f) (I a) = I (\x -> merge'' (S n f) (a x))
merge'' (D n f) (I a) = I (\x -> merge'' (D n f) (a x))
merge'' (I a) (S n f) = D n $ \s x -> SP (B.unsafeDrop n s) (I (\_ -> a (f s x)))
merge'' (I a) (D n f) = D n $ \s x -> let SP s' b = f s x in SP s' (merge'' (I a) b)
merge'' (S n f) (S m g) = S (n+m) (\s x -> f (B.unsafeDrop m s) (g s x))
merge'' (S n f) (D m g) = D m $ \s a -> let SP s' g' = g s a in SP s' (merge'' (S n f) g')
merge'' (D n f) (D m g) = D (n+m) $ \s a -> let SP s' g' = g s a in SP s' (merge'' (D n f) g')
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
  ei <- rtoa (runAndKeepTrack (pure x >>> plan_a)) -<< ()
  case ei of
    Right result -> returnA -< result
    Left saved_input -> do
      pushBack -< saved_input
      plan_b -< x

atrace :: GetA String ()
atrace = S 0 (\_s x -> trace x ())

failA :: String -> GetA a b
failA str = F str

pushBack :: GetA [B.ByteString] ()
pushBack = D 0 (\s bs -> SP (B.concat (bs ++ [s])) (pure ()))

rtoa :: R a -> GetA () a
rtoa (Done a) = S 0 (\_ _ -> a)
rtoa (Fail str) = F str -- should never happen if used with runAndKeepTracks
rtoa (Hungry n f) = D n (\s _ -> SP B.empty (rtoa (f s)))

runAndKeepTrack :: GetA () b -> R (Either [B.ByteString] b)
runAndKeepTrack a0 = go (run a0) []  
  where
    go a saved =
      case a of
        Done x -> Done (Right x)
        Fail _ -> Done (Left (reverse saved))
        Hungry n f -> Hungry n $ \s -> go (f s) (s:saved)

runEither :: GetA () b -> R (Either [B.ByteString] b)
runEither a = go (runAndKeepTrack' a)
  where
  	go r =
  		case r of
  			Done (Done x, _saved) -> Done (Right x)
  			Done (Fail _str, saved) -> Done (Left saved)
  			Hungry n f -> Hungry n (go . f)
  			Done (Hungry _ _, _) -> error "Binary: impossible"
  			Fail _ -> error "Binary: impossible"

runAndKeepTrack' :: GetA () b -> R (R b, [B.ByteString])
runAndKeepTrack' a = go (run a) [] 
  where
    go a saved =
      case a of
        Done x -> Done (Done x, saved)
        Fail str -> Done (Fail str, saved)
        Hungry n f -> Hungry n $ \s -> go (f s) (s:saved)

runSimple :: GetA () a -> B.ByteString -> a
runSimple !a s0 =
	case runChunk a s0 of
		Fail str -> error $ "Binary: Failed miserably: " ++ str
		Done x -> x
		Hungry _ _ -> error "Binary: requested more input than available"
{-# INLINE runSimple #-}

data R a = Done a
         | Fail String
         | Hungry {-# UNPACK #-} !Int (B.ByteString -> R a)

instance Show a => Show (R a) where
  show (Done a) = "Done: " ++ show a
  show (Fail str) = "Fail: " ++ str
  show (Hungry n _) = "Hungry for at least " ++ show n ++ " more bytes"

runtimeInfo :: GetA () RuntimeInfo
runtimeInfo = I (\_ -> S 0 (\bs rti -> rti))
{-# INLINE runtimeInfo #-}

testRuntimeInfo :: GetA () (String, RuntimeInfo)
testRuntimeInfo = proc _ -> do
	str <- returnA -< "hej"
	rti <- runtimeInfo -< ()
	returnA -< (str, rti)

run :: GetA () b -> R b
run a = runChunk a B.empty

runChunk :: GetA () a -> B.ByteString -> R a
runChunk a bs = runContinue 0 True a bs
{-# INLINE runChunk #-}

runContinue :: Int -> Bool -> GetA () a -> B.ByteString -> R a
runContinue pos haveMore !a s0 = --trace (show a) $
  case a of
    F str -> Fail str
    S n f | B.length s0 >= n -> Done (f s0 ())
          | otherwise -> Hungry (n - B.length s0) $ \s ->
              runContinue (pos + B.length s) haveMore a (B.append s0 s)
    D n f | B.length s0 >= n ->
              case f s0 () of SP s' a' -> runContinue pos haveMore a' s'
          | otherwise -> Hungry (n - B.length s0) $ \s ->
              runContinue (pos + B.length s) haveMore a (B.append s0 s)
    I f -> let rti = RTI pos haveMore
           in runContinue pos haveMore (pure rti >>> f ()) s0

{-# LANGUAGE Arrows, BangPatterns #-}

module Main (main, test, ktimes, getStruct4, fooi, testChoice, hej4) where

import Data.Binary.Get.Arrow as GetA

import Control.Arrow

import Control.DeepSeq

import Data.Word
import Data.Int
import Data.Char

import Criterion.Main as C

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import qualified Data.Binary.Get as BG

main :: IO ()
main = do
  let !lbs_input = L.fromChunks [input]
  C.defaultMain
    [
      C.bench "static" $ C.whnf (runSimple test) input,
      C.bench "binary static" $ C.nf (BG.runGet binary_test) lbs_input,
      C.bench "binary getStruct4" $ (C.nf (BG.runGet (getStruct4_binary mega))) oneMegabyteLBS,
      C.bench "getStruct4" $ C.nf (runSimple (getStruct4 mega)) oneMegabyte,
      C.bench "run fooi input" $ C.whnf (runSimple fooi) input,
      C.bench "run hej input" $ C.whnf (runSimple (hej 5)) input,
      C.bench "run hej2 input" $ C.whnf (runSimple (hej2 5)) input,
      C.bench "run hej3 input" $ C.whnf (runSimple (hej3 5)) input,
      C.bench "hej4" $ C.whnf (runSimple (hej4 mega)) oneMegabyte
    ]

oneMegabyte :: B.ByteString
oneMegabyte = B.replicate mega $ fromIntegral $ ord 'a'

oneMegabyteLBS :: L.ByteString
oneMegabyteLBS = L.fromChunks [oneMegabyte]

mega = 1024 * 1024

input = B.pack [1..5]
varinput = B.pack [0x80, 0xa0, 0x11, 0x80, 0xa0, 0x11]

ktimes :: (b -> a) -> b -> a
ktimes f str = go (1000 :: Int) (f str) str
  where
    go 0 !v !str = v
    go n !_ !str = go (n-1) (f str) str
{-# INLINE ktimes #-}

mtimes :: (b -> a) -> b -> a
mtimes f str = go (1000000 :: Int) (f str) str
  where
    go 0 !v !str = v
    go n !_ !str = go (n-1) (f str) str
{-# INLINE mtimes #-}

varInt32_2 :: GetA () Int32
varInt32_2 = proc _ -> do
  a <- varInt32 -< ()
  b <- varInt32 -< ()
  returnA -< a + b

ktest = ktimes . runSimple
mtest = mtimes . runSimple

data Struct4 = Struct4 {-# UNPACK #-} !Word8
                       {-# UNPACK #-} !Word8
                       {-# UNPACK #-} !Word8
                       {-# UNPACK #-} !Word8
                deriving Show

instance NFData Struct4 where
  rnf a@(Struct4 _ _ _ _) = a `seq` ()

getStruct4 mega = list (mega `div` 4) go
  where
    go = proc _ -> do
      w0 <- word8 -< ()
      w1 <- word8 -< ()
      w2 <- word8 -< ()
      w3 <- word8 -< ()
      let !st = Struct4 w0 w1 w2 w3
      returnA -< st

getStruct4_binary = loop []
  where loop acc 0 = return acc
        loop acc n = do
          !w0 <- BG.getWord8
          !w1 <- BG.getWord8
          !w2 <- BG.getWord8
          !w3 <- BG.getWord8
          let !st = Struct4 w0 w1 w2 w3
          loop (st : acc) (n - 4)

testChoice :: GetA () (Word16)
testChoice = proc _ -> do
  w <- word8 -< ()
  if w == 0
    then word16be -< ()
    else word16le -< ()

binary_test :: BG.Get Word8
binary_test = do
    !a <- BG.getWord8
    !b <- BG.getWord8
    !c <- BG.getWord8
    !d <- BG.getWord8
    let !x = a + b + c +d
    return x

test :: GetA t Word8
test =
  proc _ -> do
    !a <- word8 -< ()
    !b <- word8 -< ()
    !c <- word8 -< ()
    !d <- word8 -< ()
    let !x = a + b + c +d
    returnA -< x

fooi :: GetA t (Word8, Word8, B.ByteString, B.ByteString)
fooi =
  proc _ -> do
    !a <- word8 -< ()
    !b <- word8 -< ()
    !c <- string (fromIntegral a) -<< ()
    !d <- string 2 -< ()
    returnA -< (a,b,c,d)

hej :: Int -> GetA () Word8
hej n0 = proc _ -> go n0 -< 0
  where
    go 0 = proc acc -> returnA -< acc
    go n = proc acc -> do
      !a <- word8 -< ()
      let !x = a + acc
      go (n - 1) -< x

hej2 :: Int -> GetA () Word8
hej2 n = proc _ -> do
  xs <- list n word8 -< ()
  returnA -< sum xs

hej3 :: Int -> GetA () Word8
hej3 n = GetA.foldr n (+) 0 word8

hej4 :: Int -> GetA () Word32
hej4 n = GetA.foldr (n`div`4) (+) 0 word32le

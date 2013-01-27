
module Decoder (benchmarks, decodeWord32le, decodeWord32be) where

import Data.Binary.Get.Arrow as GetA

import Data.Word
import Data.Char

import Criterion.Main as C

import qualified Data.ByteString as B

benchmarks :: Benchmark
benchmarks = C.bgroup "Decoder"
  [
    C.bench "word8" (C.whnf (runSimple (decodeWord8 mega)) oneMegabyte),
    C.bench "word16le" (C.whnf (runSimple (decodeWord16le mega)) oneMegabyte),
    C.bench "word16be" (C.whnf (runSimple (decodeWord16be mega)) oneMegabyte),
    C.bench "word32le" (C.whnf (runSimple (decodeWord32le mega)) oneMegabyte),
    C.bench "word32be" (C.whnf (runSimple (decodeWord32be mega)) oneMegabyte),
    C.bench "word64le" (C.whnf (runSimple (decodeWord64le mega)) oneMegabyte),
    C.bench "word64be" (C.whnf (runSimple (decodeWord64be mega)) oneMegabyte)
  ]

oneMegabyte :: B.ByteString
oneMegabyte = B.replicate mega $ fromIntegral $ ord 'a'

mega :: Int
mega = 1024 * 1024

decodeWord8 :: Int -> GetA () Word8
decodeWord8 n = GetA.foldl' n (+) 0 word8

decodeWord16le :: Int -> GetA () Word16
decodeWord16le n = GetA.foldl' (n`div`2) (+) 0 word16le

decodeWord16be :: Int -> GetA () Word16
decodeWord16be n = GetA.foldl' (n`div`2) (+) 0 word16be

decodeWord32le :: Int -> GetA () Word32
decodeWord32le n = GetA.foldl' (n`div`4) (+) 0 word32le

decodeWord32be :: Int -> GetA () Word32
decodeWord32be n = GetA.foldl' (n`div`4) (+) 0 word32be

decodeWord64le :: Int -> GetA () Word64
decodeWord64le n = GetA.foldl' (n`div`8) (+) 0 word64le

decodeWord64be :: Int -> GetA () Word64
decodeWord64be n = GetA.foldl' (n`div`8) (+) 0 word64be
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Criterion.Main
import           System.Random

import           Aes.Hypothesis
import           AesImport
import           AesReference
import           Masking

import           Aes.QuickCheck
import           Aes.Random

main :: IO ()
main = do
  criterionRandom
  -- computeFirstSBOX thekey thetext
  -- compute_100000_CPA_hypothesis


benchRandom :: Int -> [Plaintext]
benchRandom n = take n $ randoms (mkStdGen 0)

benchQuickCheck :: Int -> IO [Plaintext]
benchQuickCheck n = take n <$> generate infiniteList

criterionRandom :: IO ()
criterionRandom = defaultMain
  [ bgroup "random"
    [ bench "1000"    $ nf benchRandom 1000
    , bench "10000"   $ nf benchRandom 10000
    , bench "100000"  $ nf benchRandom 100000
    ]
  , bgroup "quickcheck"
    [ bench "1000"    $ nfIO $ benchQuickCheck 1000
    , bench "10000"   $ nfIO $ benchQuickCheck 10000
    , bench "100000"  $ nfIO $ benchQuickCheck 100000
    ]
  ]

-- | compute the output of the first SBOX
computeFirstSBOX :: Key -> Plaintext -> IO ()
computeFirstSBOX k t = do
  print $ aesBlockEncrypt k t
  print $ aesMaskedBlockEncrypt
    (PreSubBytesMask $ Mask8 0x13)
    (PostSubBytesMask $ Mask8 0x7A)
    (MixColumnMask 0x55 0xAA 0x69 0x8B)
    k t

thekey :: Key
thekey = stringImport "1 35 69 103 137 171 205 239 18 52 86 120 154 188 222 240"

thetext :: Plaintext
thetext = stringImport "0 17 34 51 68 85 102 119 136 153 170 187 204 221 238 255"

compute_100000_CPA_hypothesis :: IO ()
compute_100000_CPA_hypothesis = do
  texts <- importTexts "plaintexts.txt"
  let hyps = hammingWeight $ firstRoundSBOX 0 texts
  exportHypothesis "test.txt" hyps

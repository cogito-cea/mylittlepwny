{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List      (partition)

import           Aes
import           Aes.Hypothesis
import           AesImport
import           Masking


main :: IO ()
main = do
  -- computeFirstSBOX thekey thetext
  -- compute100000CPAHypothesis
  take 100000 <$> randomPlaintexts >>= exportTexts "output.txt"

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

compute100000CPAHypothesis :: IO ()
compute100000CPAHypothesis = do
  texts <- importTexts "plaintexts.txt"
  let hyps = hammingWeight $ firstRoundSBOX 0 texts
  exportHypothesis "test.txt" hyps

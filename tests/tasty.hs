{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

import           Test.SmallCheck.Series
import           Test.SmallCheck.Series.Instances
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck

import           Data.Bits
import           Data.Word
import           GHC.Generics

import           AesImport
import           AesReference
import           Masking

import           Aes.Types

instance Monad m => Serial m Mask8 where
  series = newtypeCons Mask8
instance Monad m => Serial m PreSubBytesMask where
  series = newtypeCons PreSubBytesMask
instance Monad m => Serial m PostSubBytesMask where
  series = newtypeCons PostSubBytesMask
instance Monad m => Serial m MixColumnMask where
  series = cons4 MixColumnMask

instance Monad m => Serial m Plaintext where
  series = newtypeCons Plaintext

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ unitTests
                          , aesProperties
                          , ieProperties
                          ]

unitTests = testGroup "Unit tests: excerpt from the NIST test suite"
  [ testCase "reference AES, ECB mode, 128bit key" $
    aesBlockEncrypt k t @?= c
  , testCase "masked AES,    ECB mode, 128bit key" $
    aesMaskedBlockEncrypt m m' mc k t @?= c
  ]
  where
    t :: Plaintext
    t = stringImport "0 17 34 51 68 85 102 119 136 153 170 187 204 221 238 255"

    k :: Key
    k = stringImport "00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15"
    c :: Ciphertext
    c = stringImport "105 196 224 216 106 123 4 48 216 205 183 128 112 180 197 90"

    m = PreSubBytesMask $ Mask8 0x13
    m' = PostSubBytesMask $ Mask8 0x7A
    mc = MixColumnMask 0x55 0xAA 0x69 0x8B

aesProperties :: TestTree
aesProperties = testGroup "Properties: first order masking scheme"
  [ testProperty "a simple property-based test to validate this test program" $
    forAll $ \a b r ->
      (a :: Word8) `xor` (b :: Word8) == a `xor` (r :: Word8) `xor` b `xor` r
  , testProperty "equality for ECB encryption with 128-bit keys" $
    forAll $ \m m' mc ->
      aesMaskedBlockEncrypt m m' mc k t == aesBlockEncrypt k t
  ]
  where
    t :: Plaintext
    t = stringImport "0 17 34 51 68 85 102 119 136 153 170 187 204 221 238 255"
    k :: Key
    k = stringImport "00 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15"

ieProperties :: TestTree
ieProperties = testGroup "Properties: import and export functions"
  [ testProperty "identity of importTexts . exportTexts" $
    forAll $ \t ->
      stringImport (exportString t) == (t :: Plaintext)
  ]

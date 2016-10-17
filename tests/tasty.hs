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

import           AesReference
import           Masking

newtype DeleteThis = DeleteThis Int
  deriving (Eq, Show)
instance Monad m => Serial m DeleteThis where
    series = newtypeCons DeleteThis

instance Monad m => Serial m Mask8 where
    series = newtypeCons Mask8
instance Monad m => Serial m PreSubBytesMask where
    series = newtypeCons PreSubBytesMask
instance Monad m => Serial m PostSubBytesMask where
    series = newtypeCons PostSubBytesMask
instance Monad m => Serial m MixColumnMask where
    series = cons4 MixColumnMask

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ unitTests
                          , properties
                          ]

unitTests = testGroup "Unit tests: excerpt from the NIST test suite"
  [ testCase "reference AES, ECB mode, 128bit key" $
    aesBlockEncrypt key plaintext @?= cipher
  , testCase "masked AES,    ECB mode, 128bit key" $
    aesMaskedBlockEncrypt m m' mc key plaintext @?= cipher
  ]
  where
    plaintext = Plaintext [ 0x00, 0x11, 0x22, 0x33
                          , 0x44, 0x55, 0x66, 0x77
                          , 0x88, 0x99, 0xaa, 0xbb
                          , 0xcc, 0xdd, 0xee, 0xff ]
    key = Key128 $ RawKey [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f]
    cipher = Ciphertext [ 0x69, 0xc4, 0xe0, 0xd8
                        , 0x6a, 0x7b, 0x04, 0x30
                        , 0xd8, 0xcd, 0xb7, 0x80
                        , 0x70, 0xb4, 0xc5, 0x5a]
    m = PreSubBytesMask $ Mask8 0x13
    m' = PostSubBytesMask $ Mask8 0x7A
    mc = MixColumnMask 0x55 0xAA 0x69 0x8B

properties :: TestTree
properties = testGroup "Properties: first order masking scheme"
  [ testProperty "a simple property-based test to validate this test program" $
    forAll $ \a b r -> (a :: Word8) `xor` (b :: Word8) == a `xor` (r :: Word8) `xor` b `xor` r
  , testProperty "equality for ECB encryption with 128-bit keys" $
    forAll $ \m m' mc ->
      aesMaskedBlockEncrypt m m' mc key plaintext == aesBlockEncrypt key plaintext
  ]
  where
    plaintext = Plaintext [ 0x00, 0x11, 0x22, 0x33
                          , 0x44, 0x55, 0x66, 0x77
                          , 0x88, 0x99, 0xaa, 0xbb
                          , 0xcc, 0xdd, 0xee, 0xff ]
    key = Key128 $ RawKey [0x00010203, 0x04050607, 0x08090a0b, 0x0c0d0e0f]

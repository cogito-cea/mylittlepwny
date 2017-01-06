{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

import           Test.SmallCheck.Series
import           Test.SmallCheck.Series.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck

import           Data.Bits                        (shiftL, shiftR, xor, (.&.))
import           Data.Word
import           GHC.Generics                     ()

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
                          , bitProperties
                          ]

unitTests :: TestTree
unitTests = testGroup "Unit tests: excerpt from the NIST test suite"
  [ testCase "reference AES, ECB mode, 128bit key" $
    aesBlockEncrypt k t @?= c
  , testCase "masked AES,    ECB mode, 128bit key" $
    aesMaskedBlockEncrypt m m' mc k t @?= c
  ]
  where
    t :: Plaintext
    t = stringImport "255 238 221 204 187 170 153 136 119 102 85 68 51 34 17 0"

    k :: Key
    k = stringImport "15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00"
    c :: Ciphertext
    c = stringImport "90 197 180 112 128 183 205 216 48 4 123 106 216 224 196 105"

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

bitProperties :: TestTree
bitProperties = testGroup "Properties: Aes.Bits'"
  [
    -- Plaintext values.  Cannot test bit values above 7 because
    -- currently Plaintext uses a list of Word8 of variable length.
    -- For small values of x, the list of Word8 values may contain
    -- only one element.
    testProperty "Plaintext: function 'bit', b = 0" $
    forAll $ \x ->
      let t = stringImport (show x) :: Plaintext
      in  x `shiftR` 0 .&. 0x1 == bit 0 t
  , testProperty "Plaintext: function 'bit', b = 1" $
    forAll $ \x ->
      let t = stringImport (show x) :: Plaintext
      in  x `shiftR` 1 .&. 0x1 == bit 1 t
  , testProperty "Plaintext: function 'bit', b = 7" $
    forAll $ \x ->
      let t = stringImport (show x) :: Plaintext
      in  x `shiftR` 7 .&. 0x1 == bit 7 t
  -- Plaintext values.  Make sure that the list of Word8 values in
  -- Plaintext has at least two elements.
  --
  -- TODO how to add conditions on the value of x? (e.g. x > 0, x > 7)
  , testProperty "Plaintext: function 'bit', b = 7" $
    forAll $ \x ->
      let x' = (abs x) `shiftL` 8 + 1
          t = stringImport (show x') :: Plaintext
      in  x' `shiftR` 7 .&. 0x1 == bit 7 t

  -- State. stringImport cannot import a state value.
  ]

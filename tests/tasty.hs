{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables  #-}

import           Test.SmallCheck.Series
import           Test.SmallCheck.Series.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck

import           Data.Bits                        (shiftL, shiftR, xor, (.&.))
import           Data.Word
import           GHC.Generics                     ()

import           Aes
import           Aes.Hypothesis                   (firstSBOX)
import           AesImport
import           Masking
import           TTest

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

instance Monad m => Serial m BitNumber where
  -- We are interested in integer bit-values up to 256 elements only
  -- (in the case of AES-256), no more.
  -- 
  -- For negative values on 'n', the generate function will return [].
  series = generate $
           \n -> bitNumber <$> if n < 256 then [0..n] else [0..256]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [ unitTests
                          , aesProperties
                          , ieProperties
                          , bitProperties
                          , ttestProperties
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
bitProperties = testGroup "Properties: Aes.Bits"
  [
    testProperty "Plaintext: function 'bit'" $
    forAll $ \x bitnb ->
      let x' = getNonNegative x
          t = stringImport (show x) :: Plaintext
      in  x' `shiftR` number bitnb .&. 0x1 == bit bitnb t

  -- State. stringImport cannot import a state value.
  ]

ttestProperties :: TestTree
ttestProperties = testGroup "Properties: t-tests"
  [
    -- check that the two populations of t-tests produce correct intermediate values.
    testProperty "t-test, random vs. random.  using the first SBOX output of AES" $
    -- 'monadic' executes the test in the IO monad
    forAll $ \(b :: BitNumber) -> monadic $ do
      (pop0, pop1) <- ttestRR firstSBOX k b
      let t0 = head pop0
          t1 = head pop1
      return $ (&&) (bit b (firstSBOX k t0) == 0) (bit b (firstSBOX k t1) == 1)
  ]
  where
    k :: Key
    k = stringImport "15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00"
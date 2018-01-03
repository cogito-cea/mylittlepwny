{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}

import           Test.SmallCheck.Series
import           Test.SmallCheck.Series.Instances ()
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.SmallCheck

import           Data.Bits                        (shiftL, shiftR, xor, (.&.))
import           Data.Word
import           GHC.Generics                     ()

import           Aes
import           Aes.Hypothesis                   (firstSBOXHyps, firstSBOXHypsPartial)
import           AesImport
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

-- TODO Plaintext has a fixed length, whereas in this instanciation a
--      plaintext can have any length.
instance Monad m => Serial m Plaintext where
  series = newtypeCons Plaintext

instance Monad m => Serial m Ciphertext where
  series = newtypeCons Ciphertext

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
                          , hypothesisProperties
                          ]

unitTests :: TestTree
unitTests = testGroup "Unit tests: excerpt from the FIPS 197 test suite"
  [ testCase "reference AES, ECB mode, 128bit key" $
    aesBlockEncrypt k t @?= c
  , testCase "masked AES,    ECB mode, 128bit key" $
    aesMaskedBlockEncrypt m m' mc k t @?= c
  , testCase "Annex B -- cipher example. 128bit key" $
    aesBlockEncrypt cipherk input @?= c'
  ]
  where
    t :: Plaintext
    t = stringImport "0x00 0x11 0x22 0x33 0x44 0x55 0x66 0x77 0x88 0x99 0xaa 0xbb 0xcc 0xdd 0xee 0xff"

    k :: Key
    k = stringImport "0x00 0x01 0x02 0x03 0x04 0x05 0x06 0x07 0x08 0x09 0x0a 0x0b 0x0c 0x0d 0x0e 0x0f"

    c :: Ciphertext
    c = stringImport "0x69 0xc4 0xe0 0xd8 0x6a 0x7b 0x04 0x30 0xd8 0xcd 0xb7 0x80 0x70 0xb4 0xc5 0x5a"

    m = PreSubBytesMask $ Mask8 0x13
    m' = PostSubBytesMask $ Mask8 0x7A
    mc = MixColumnMask 0x55 0xAA 0x69 0x8B

    input :: Plaintext
    input = stringImport "0x32 0x43 0xf6 0xa8 0x88 0x5a 0x30 0x8d 0x31 0x31 0x98 0xa2 0xe0 0x37 0x07 0x34"

    cipherk :: Key
    cipherk = stringImport "0x2b 0x7e 0x15 0x16 0x28 0xae 0xd2 0xa6 0xab 0xf7 0x15 0x88 0x09 0xcf 0x4f 0x3c"

    c' :: Ciphertext
    c' = stringImport "0x39 0x25 0x84 0x1d 0x02 0xdc 0x09 0xfb 0xdc 0x11 0x85 0x97 0x19 0x6a 0x0b 0x32"

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
  [ testProperty "identity of fromAesText . toAesText :: Plaintext" $
    forAll $ \x ->
      (fromAesText . toAesText) x == (x :: Plaintext)
  , testProperty "identity of fromAesText . toAesText :: Ciphertext" $
    forAll $ \x ->
      (fromAesText . toAesText) x == (x :: Ciphertext)
  , testProperty "identity of importTexts . exportTexts :: Plaintext" $
    forAll $ \t ->
      stringImport (exportString t) == (t :: Plaintext)
  , testProperty "identity of importTexts . exportTexts :: Ciphertext" $
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

hypothesisProperties :: TestTree
hypothesisProperties = testGroup "Properties: Aes.Hypothesis"
  [
    testProperty "Hypothesis: output of the first SBOX" $
    forAll $ \byte ->
      firstSBOXHyps byte txt == firstSBOXHypsPartial byte [0..255] txt
  ]
  where
    txt :: Plaintext
    txt = stringImport "0 17 34 51 68 85 102 119 136 153 170 187 204 221 238 255"

-- fstSBOX :: Byte -> [Word8] -> Plaintext -> [Word8]
-- firstRoundSBOX :: Byte -> Plaintext -> [Word8]
-- firstSBOX :: Key -> Plaintext -> State

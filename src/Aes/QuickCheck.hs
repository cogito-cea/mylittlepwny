module Aes.QuickCheck
  ( Arbitrary
  , generate
  , infiniteList
  ) where

import           Test.QuickCheck

import           Aes.Types

-- | a Plaintext is a list of Word8
--
--  λ> sample (arbitrary:: Gen Plaintext)
-- Plaintext [1,1,0,1,0,0,0,0,0,0,0,1,1,1,0,1]
-- Plaintext [1,1,1,1,1,1,0,1,0,0,1,1,0,0,0,0]
-- Plaintext [2,0,0,0,1,2,2,1,2,0,2,1,1,2,1,0]
-- Plaintext [1,1,3,1,1,1,3,3,2,3,1,4,0,1,0,3]
-- Plaintext [2,0,1,7,8,8,3,5,8,0,6,8,1,3,0,3]
-- Plaintext [10,13,11,2,1,3,11,1,15,4,13,10,15,12,12,10]
-- Plaintext [15,6,8,14,3,10,16,14,11,4,1,7,0,10,6,14]
-- Plaintext [5,6,15,6,21,19,17,13,31,19,8,25,22,9,31,16]
-- Plaintext [59,61,43,49,31,7,22,22,2,24,9,43,58,26,45,7]
-- Plaintext [37,30,53,33,88,29,52,93,108,88,108,83,2,31,98,101]
-- Plaintext [216,33,220,84,35,37,4,101,26,179,57,168,90,103,250,78]

instance Arbitrary Plaintext where
  arbitrary = Plaintext <$> vectorOf 16 arbitrary


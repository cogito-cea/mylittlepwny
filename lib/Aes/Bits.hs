{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Description of bit positions in an integral value. The main
-- purpose of this module is to provide the type 'BitPos' and
-- associated helpers.
--
-- This module is different from 'Data.Bits', which provides bitwise
-- functions over integral types.

module Aes.Bits where

import           Control.DeepSeq (NFData)
import           Data.Bits (shiftR, (.&.), Bits)

import           Aes.Types

-- | a bit value extracted from Aes input, intermediate or output variables
newtype Bit = Bit Int
  deriving (Bits, Eq, Ord, Read, Show, Enum, Num, NFData)

-- | The bit position of a bit variable in a bit field.  Bit numbering
--   starts at 0, form LSB.
newtype BitPos = BitPos Int
  deriving (Bits, Eq, Ord, Read, Show, Enum, Num)

class HasBitPos a where
  bit :: BitPos -> a -> Bit

instance HasBitPos Plaintext where
  bit = bitPosPt

instance HasBitPos State where
  bit = bitPosSt

-- | Extract the 'n'th bit of the input data. 'Plaintext' stores
-- 'Word8' values with the _least_ significant first, i.e.
-- [p0, p1, p2...].
bitPosPt :: BitPos -> Plaintext -> Bit
bitPosPt (BitPos n) (Plaintext ws)
  | bytePos >= 0 = Bit $ fromEnum (reverse ws !! bytePos) `shiftR` r .&. 0x1
  | otherwise    = 0
  where l      = length ws
        -- a Plaintext is a list of Word8 values, i.e. a list of 8-bit
        -- long values.
        (q, r) = quotRem n 8
        -- is the byte element available in the list or not ?
        bytePos = l-q-1

-- | Extract the 'n'th bit of the input data. 'State' stores
-- 'Word32' values... in an ankward way.  Actually using a Big Endian thingee.
bitPosSt :: BitPos -> State -> Bit
bitPosSt (BitPos n) (State w0 w1 w2 w3 _)
  | bytePos >= 0 = toEnum $ fromEnum (ws !! bytePos) `shiftR` r .&. 0x1
  | otherwise    = 0
  where ws = [w3, w2, w1, w0]
        l = length ws
        -- a Plaintext is a list of Word32 values, i.e. a list of 32-bit
        -- long values.
        (q, r) = quotRem n 32
        bytePos = l-q-1

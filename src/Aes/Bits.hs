{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Aes.Bits
  ( HasBit
  , BitNumber
  , bit
  )
  where

import           Data.Bits (shiftR, (.&.))

import           Aes.Types

-- | a bit value extracted from Aes input, intermediate or output variables
type Bit = Int

-- | The bit position of a bit variable in a bit field.  Bit numbering
--   starts at 0, form LSB.
newtype BitNumber = BitNumber Int
  deriving (Read, Show, Enum)

class HasBit a where
  bit :: BitNumber -> a -> Bit

instance HasBit Plaintext where
  bit = bitPt

instance HasBit State where
  bit = bitSt

-- | Extract the 'n'th bit of the input data. 'Plaintext' stores
-- 'Word8' values with the _least_ significant first, i.e.
-- [p0, p1, p2...].
bitPt :: BitNumber -> Plaintext -> Bit
bitPt (BitNumber n) (Plaintext ws)
  | bytePos >= 0 = fromEnum (reverse ws !! bytePos) `shiftR` r .&. 0x1
  | otherwise    = 0
  where l      = length ws
        -- a Plaintext is a list of Word8 values, i.e. a list of 8-bit
        -- long values.
        (q, r) = quotRem n 8
        -- is the byte element available in the list or not ?
        bytePos = l-q-1

-- | Extract the 'n'th bit of the input data. 'State' stores
-- 'Word32' values... in an ankward way.  Actually using a Big Endian thingee.
bitSt :: BitNumber -> State -> Bit
bitSt (BitNumber n) (State w0 w1 w2 w3 _)
  | bytePos >= 0 = fromEnum (ws !! bytePos) `shiftR` r .&. 0x1
  | otherwise    = 0
  where ws = [w3, w2, w1, w0]
        l = length ws
        -- a Plaintext is a list of Word32 values, i.e. a list of 32-bit
        -- long values.
        (q, r) = quotRem n 32
        bytePos = l-q-1

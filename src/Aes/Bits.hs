module Aes.Bits
  ( HasBit
  , bit
  )
  where

import           Data.Bits (shiftR, (.&.))

import           Aes.Types

-- | a bit value extracted from Aes input, intermediate or output variables
type Bit = Int

class HasBit a where
  bit :: Int -> a -> Bit

instance HasBit Plaintext where
  bit = bitPt

instance HasBit State where
  bit = bitSt

-- Extract the 'n'th bit of the input data. 'Plaintext' stores
-- 'Word8' values with the most significant first.
bitPt :: Int -> Plaintext -> Bit
bitPt n (Plaintext ws) = let l = length ws
                             (q, r) = quotRem n 8
                         in  fromEnum (ws !! (l-q-1)) `shiftR` r .&. 0x1
-- Extract the 'n'th bit of the input data. 'State' stores
-- 'Word32' values with the most significant first.
bitSt :: Int -> State -> Bit
bitSt n (State w0 w1 w2 w3 _) =
  let ws = [w0, w1, w2, w3]
      l = length ws
      (q, r) = quotRem n 32
  in  fromEnum (ws !! (l-q-1)) `shiftR` r .&. 0x1
